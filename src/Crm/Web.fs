module Crm.Web
open System
open System.IO
open System.Text

open FSharpPlus.AspNetCore.Suave
open Filters
open Successful
open RequestErrors

open Fleece
open Fleece.FSharpData
open Fleece.FSharpData.Operators
open FSharp.Data

open FSharpPlus
open FSharpPlus.Operators

open Crm
open Crm.Domain

(* Fake auth in order to simplify web testing *)

type Session =
  | NoSession
  | UserLoggedOn of UserId
type JwtPayload = { subject:string }
with
  static member OfJson json:ParseResult<JwtPayload> =
    let create sub= { subject =sub }
    match json with
    | JObject o -> create <!> (o .@ "sub")
    | x -> Decode.Fail.objExpected x
let ``x-jwt-payload authenticated`` f = //
  let context apply (a : Http.Context) = apply a a
  context (fun x ->
    match Request.Header.tryGet "x-jwt-payload" x.request with
    | Some u ->
      string u
      |> Convert.FromBase64String
      |> Encoding.UTF8.GetString
      |> parseJson
      |> Result.map( fun (payload:JwtPayload)->UserId payload.subject)
      |> function | Ok user->f (UserLoggedOn(user))
                  | Error _ ->f NoSession
    | None -> f NoSession)

module Json=
  open Writers
  let inline OK v=
    OK (string v)
    >=> setContentType "application/json; charset=utf-8"
  let inline CREATED v=
    CREATED (string v)
    >=> setContentType "application/json; charset=utf-8"
  let inline BAD_REQUEST v =
    BAD_REQUEST (string v)
    >=> setContentType "application/json; charset=utf-8"
  let getBody (c:Http.Context)=
    use reader =new StreamReader(c.request.Body)
    let body = reader.ReadToEnd()
    try Some (JsonValue.Parse body) with _ -> None

module Paths =
  type Int64Path = PrintfFormat<int64 -> string, unit, string, string, int64>
  type Int64AndInt64Path = PrintfFormat<int64 -> int64 -> string, unit, string, string, (int64*int64)>
  /// /contacts
  let contacts = "/contacts"
  /// /contacts/INT
  let contact : Int64Path = "/contacts/%d"
  /// /contacts/INT/contacts
  let associatedContacts : Int64Path = "/contacts/%d/contacts"
  let associateContact : Int64AndInt64Path = "/contacts/%d/contacts/%d"
  /// /contacts/INT/activities
  let activities : Int64Path = "/contacts/%d/activities"
  /// /contacts/INT/activities/INT
  let activity : Int64AndInt64Path = "/contacts/%d/activities/%d"
  /// /contacts/INT/comments/INT
  let comments : Int64Path = "/contacts/%d/comments"
  /// /contacts/INT/comments/INT
  let comment : Int64AndInt64Path = "/contacts/%d/comments/%d"

(* Json API *)

module JsonValue=
  let ofList = List.toArray >> JsonValue.Array
module ToJson=
  let comment (ContactId contactId) (comment: Comment) :JsonValue =
    jobj [
      "commentId" .= comment.commentId
      "commentUri" .= (sprintf Paths.comment contactId comment.commentId)
      "comment" .= comment.comment
    ]

  let activity (ContactId contactId) (activity: Activity) :JsonValue =
    jobj [
      "activityId" .= activity.activityId
      "activityUri" .= (sprintf Paths.activity contactId activity.activityId)
      "description" .= activity.description
      "at" .= activity.at
      "tags" .= activity.tags
    ]
  let contactProperties (contact: Contact)=
    let activities = List.map (activity contact.contactId) contact.activities |> JsonValue.ofList
    let comments = List.map (comment contact.contactId) contact.comments |> JsonValue.ofList
    [
      "contactId" .= ContactId.unwrap contact.contactId
      "contactUri" .= (sprintf Paths.contact <| ContactId.unwrap contact.contactId)
      "name" .= contact.name
      "phone" .= contact.phone
      "email" .= contact.email
      "tags" .= contact.tags
      "activities", activities
      "comments", comments
    ]
  let contact (contact: Contact) :JsonValue =
    jobj <| contactProperties contact
  let associatedContact (contact: Contact, tag) :JsonValue =
    jobj <| ["type" .= tag] @ (contactProperties contact)
module OfJson=
  let contactReq context json : Contact ParseResult =
    let create name phone email tags= {
      contactId = CommandContext.idOf name context |> ContactId
      name=name
      phone=Option.defaultValue "" phone; email=Option.defaultValue "" email
      tags=Option.defaultValue [] tags
      comments=[]
      activities=[]
    }
    match json with
    | JObject o -> create <!> (o .@ "name") <*> (o .@? "phone") <*> (o .@? "email")<*> (o .@? "tags")
    | x -> Decode.Fail.objExpected x
  let commentReq context json: Comment ParseResult =
    let create comment = {
      commentId = CommandContext.idOf comment context
      comment = comment
    }
    match json with
    | JObject o -> create <!> (o .@ "comment")
    | x -> Decode.Fail.objExpected x
  let activityReq context json: Activity ParseResult =
    let create desc at tags = {
      activityId = CommandContext.idOf desc context
      description =desc; at=at; tags=Option.defaultValue [] tags
    }
    match json with
    | JObject o -> create <!> (o .@ "description") <*> (o .@ "at") <*> (o .@? "tags")
    | x -> Decode.Fail.objExpected x

  let updateContactReq (contactId)  _ json:  (ContactId * string option* string option* string option * string list option) ParseResult=
    //name:string * phone:string * email:string
    let create name phone email tags= (contactId, name, phone, email, tags)
    match json with
    | JObject o -> create <!> (o .@? "name") <*> (o .@? "phone") <*> (o .@? "email") <*> (o .@? "tags")
    | x -> Decode.Fail.objExpected x

  let updateActivityReq contactId activityId _ json: (ContactId * ActivityId * string option* DateTime option* string list option) ParseResult =
    let create desc at tags = (contactId, activityId, desc,at,tags)
    match json with
    | JObject o -> create <!> (o .@? "description") <*> (o .@? "at") <*> (o .@? "tags")
    | x -> Decode.Fail.objExpected x

  let associateReq contactId otherContactId _ json : (ContactId*string*ContactId) ParseResult=
    //AssociateContactToContact (from,association,to')
    let create typ  = (contactId, typ, otherContactId)
    match json with
    | JObject o -> create <!> (o .@ "type")
    | x -> Decode.Fail.objExpected x

let webPart authenticated (repository : IContactRepository) onCommand (time:unit->DateTime)=
  /// handle command and add result to repository
  let ``persist_then_BAD_REQUEST_or_`` result (context:CommandContext) ofJson toCommand toJson=fun (ctx) -> monad {
    match Json.getBody ctx with
    | Some body ->
      match ofJson context body with
      | Ok parsed->
        let! res = lift (onCommand(context,toCommand parsed))
        return! result (toJson (res,parsed)) ctx
      | Error err -> return! (BAD_REQUEST (string err) ctx)
    | None -> return! (BAD_REQUEST "Unable to parse JSON" ctx) }

  let ``persist_then_OK_or_BAD_REQUEST`` context = ``persist_then_BAD_REQUEST_or_`` Json.OK context
  let ``persist_then_CREATED_or_BAD_REQUEST`` context = ``persist_then_BAD_REQUEST_or_`` Json.CREATED context

  let contacts context =
    /// overview would make more sense as a view compatible with a dashboard
    let overview =
      GET >=> fun (ctx) ->
                let contactList =repository.GetContacts() |> List.map ToJson.contact |> JsonValue.ofList
                Json.OK contactList ctx

    let get id=
      GET >=> fun (ctx) ->
                match repository.GetContact(ContactId id) |> Option.map ToJson.contact with
                | Some c->Json.OK c ctx
                | None -> NOT_FOUND "" ctx

    let create =
      POST >=> ``persist_then_CREATED_or_BAD_REQUEST`` context
                OfJson.contactReq
                AddContact // tocommand
                (snd >> ToJson.contact)

    let update contactId =
      POST >=> ``persist_then_OK_or_BAD_REQUEST`` context
                (OfJson.updateContactReq (ContactId contactId))
                UpdateContact // tocommand
                (fun _ -> JNull)

    [path Paths.contacts >=> overview
     path Paths.contacts >=> create
     pathScan Paths.contact get
     pathScan Paths.contact update]

  let activities context =

    let list id=
      GET >=> fun (ctx) ->
                let contactId = ContactId id
                match repository.GetContact contactId
                      |> Option.map (fun c->List.map (ToJson.activity contactId) c.activities |> JsonValue.ofList) with
                | Some c->Json.OK c ctx
                | None -> NOT_FOUND "" ctx

    let get (contactId,activityId)=
      GET >=> fun (ctx) ->
                let contactId = ContactId contactId
                match repository.GetContact contactId
                      |> Option.bind (Contact.activityWithId activityId) |> Option.map (ToJson.activity contactId)with
                | Some c->Json.OK c ctx
                | None -> NOT_FOUND "" ctx

    let create contactId : WebPart<_>=
      let id = ContactId contactId
      POST >=> ``persist_then_CREATED_or_BAD_REQUEST`` context
                OfJson.activityReq
                (fun activity->AddActivity(id, activity)) // tocommand
                (snd >> (ToJson.activity id))

    let update (contactId,activityId): WebPart<_>=
      POST >=> ``persist_then_OK_or_BAD_REQUEST`` context
                (OfJson.updateActivityReq (ContactId contactId) (ActivityId activityId))
                UpdateActivity // tocommand
                (fun _ -> JNull)

    [pathScan Paths.activities list
     pathScan Paths.activities create
     pathScan Paths.activity get
     pathScan Paths.activity update]

  let comments context =
    let list id=
      GET >=> fun (ctx) ->
                let contactId = ContactId id
                match repository.GetContact contactId
                      |> Option.map (fun c->List.map (ToJson.comment contactId) c.comments |> JsonValue.ofList) with
                | Some c->Json.OK c ctx
                | None -> NOT_FOUND "" ctx

    let get (contactId,commentId)=
      GET >=> fun (ctx) ->
                let contactId = ContactId contactId
                match repository.GetContact contactId
                      |> Option.bind (Contact.commentWithId commentId) |> Option.map (ToJson.comment contactId) with
                | Some c->Json.OK c ctx
                | None -> NOT_FOUND "" ctx

    let create contactId : WebPart<_>=
      let id = ContactId contactId
      POST >=> ``persist_then_CREATED_or_BAD_REQUEST`` context
                OfJson.commentReq
                (fun comment->AddComment(id, comment))  // tocommand
                (snd >> (ToJson.comment id))

    [pathScan Paths.comments list
     pathScan Paths.comments create
     pathScan Paths.comment get]

  let associations context =
    let associateContact (contactId,otherContactId): WebPart<_>=
      PUT >=> ``persist_then_OK_or_BAD_REQUEST`` context
                (OfJson.associateReq (ContactId contactId) (ContactId otherContactId))
                AssociateContactToContact // tocommand
                (fun _ -> JNull)
    let getAssociatedContacts id=
      GET >=> fun (ctx) ->
                match repository.GetContactAssociations(ContactId id)
                      |> List.map ToJson.associatedContact with
                | [] -> NOT_FOUND "" ctx
                | list->Json.OK (JsonValue.ofList list) ctx

    [pathScan Paths.associatedContacts getAssociatedContacts
     pathScan Paths.associateContact associateContact]

  WebPart.choose [ path "/" >=> (OK "")
                   authenticated (function
                    | NoSession -> UNAUTHORIZED "Not logged in"
                    | UserLoggedOn user ->
                      let context = { at= time(); userId=user }
                      WebPart.choose  (List.concat [
                                       contacts context
                                       // activities
                                       activities context
                                       // comments
                                       comments context
                                       // associate contact
                                       associations context
                                       ])
                    )]
