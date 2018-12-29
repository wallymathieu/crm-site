module Crm.Web
open System
open FSharpPlus
open Crm.Suave
open Crm.Suave.Writers
open Crm.Suave.Filters
open Crm.Suave.Successful
open Crm.Suave.RequestErrors

open Fleece
open Fleece.FSharpData
open Fleece.FSharpData.Operators
open FSharp.Data

open Crm.Domain
open Crm
open FSharpPlus.Data
open System.Text
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
    | x -> Error (sprintf "Expected JwtPayload, found %A" x)
let authenticated f = //
  let context apply (a : Suave.Http.HttpContext) = apply a a
  context (fun x ->
    match x.request.header "x-jwt-payload" with
    | Choice1Of2 u ->
      Convert.FromBase64String u
      |> Encoding.UTF8.GetString
      |> parseJson
      |> Result.map( fun (payload:JwtPayload)->UserId payload.subject)
      |> function | Ok user->f (UserLoggedOn(user))
                  | Error _ ->f NoSession
    | Choice2Of2 _ -> f NoSession)

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
  let comment (comment: Comment) :JsonValue =
    jobj [
      "comment" .= comment.comment
    ]

  let activity (activity: Activity) :JsonValue =
    jobj [
      "description" .= activity.description
      "at" .= activity.at
      "tags" .= activity.tags
    ]
  let contactProperties (contact: Contact)=
    let activities = List.map activity contact.activities |> JsonValue.ofList
    let comments = List.map comment contact.comments |> JsonValue.ofList
    [
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
    | x -> Error (sprintf "Expected contact request, found %A" x)
  let commentReq context json: Comment ParseResult =
    let create comment = {
      commentId = CommandContext.idOf comment context
      comment = comment
    }
    match json with
    | JObject o -> create <!> (o .@ "comment")
    | x -> Error (sprintf "Expected comment, found %A" x)
  let activityReq context json: Activity ParseResult =
    let create desc at tags = {
      activityId = CommandContext.idOf desc context
      description =desc; at=at; tags=Option.defaultValue [] tags
    }
    match json with
    | JObject o -> create <!> (o .@ "description") <*> (o .@ "at") <*> (o .@? "tags")
    | x -> Error (sprintf "Expected activity, found %A" x)

  let updateContactReq (contactId)  _ json:  (ContactId * string option* string option* string option * string list option) ParseResult=
    //name:string * phone:string * email:string
    let create name phone email tags= (contactId, name, phone, email, tags)
    match json with
    | JObject o -> create <!> (o .@? "name") <*> (o .@? "phone") <*> (o .@? "email") <*> (o .@? "tags")
    | x -> Error (sprintf "Expected contact request, found %A" x)

  let updateActivityReq contactId activityId _ json: (ContactId * ActivityId * string option* DateTime option* string list option) ParseResult =
    let create desc at tags = (contactId, activityId, desc,at,tags)
    match json with
    | JObject o -> create <!> (o .@? "description") <*> (o .@? "at") <*> (o .@? "tags")
    | x -> Error (sprintf "Expected activity, found %A" x)

  let associateReq contactId otherContactId _ json : (ContactId*string*ContactId) ParseResult=
    //AssociateContactToContact (from,association,to')
    let create typ  = (contactId, typ, otherContactId)
    match json with
    | JObject o -> create <!> (o .@ "type")
    | x -> Error (sprintf "Expected associate request, found %A" x)

let webPart (repository : IContactRepository) (append:CommandContext*Command->Async<unit>) (time:unit->DateTime)=
  let onCommand (context,command) = async {
    do! append(context, command)
    return repository.Handle command
  }

  /// overview would make more sense as a view compatible with a dashboard
  let overview =
    GET >=> fun (ctx) ->
              let contactList =repository.GetContacts() |> List.map ToJson.contact |> JsonValue.ofList
              Json.OK contactList ctx
  let contactDetails id=
    GET >=> fun (ctx) ->
              match repository.GetContact(ContactId id) |> Option.map ToJson.contact with
              | Some c->Json.OK c ctx
              | None -> NOT_FOUND "" ctx
  let getActivities id=
    GET >=> fun (ctx) ->
              match repository.GetContact(ContactId id)
                    |> Option.map (fun c->List.map ToJson.activity c.activities |> JsonValue.ofList) with
              | Some c->Json.OK c ctx
              | None -> NOT_FOUND "" ctx
  let getComments id=
    GET >=> fun (ctx) ->
              match repository.GetContact(ContactId id)
                    |> Option.map (fun c->List.map ToJson.comment c.comments |> JsonValue.ofList) with
              | Some c->Json.OK c ctx
              | None -> NOT_FOUND "" ctx

  let getAssociatedContacts id=
    GET >=> fun (ctx) ->
              match repository.GetContactAssociations(ContactId id)
                    |> List.map ToJson.associatedContact with
              | [] -> NOT_FOUND "" ctx
              | list->Json.OK (JsonValue.ofList list) ctx

  let getActivity (contactId,activityId)=
    GET >=> fun (ctx) ->
              match repository.GetContact(ContactId contactId)
                    |> Option.bind (Contact.activityWithId activityId) |> Option.map ToJson.activity with
              | Some c->Json.OK c ctx
              | None -> NOT_FOUND "" ctx

  let getComment (contactId,commentId)=
    GET >=> fun (ctx) ->
              match repository.GetContact(ContactId contactId)
                    |> Option.bind (Contact.commentWithId commentId) |> Option.map ToJson.comment with
              | Some c->Json.OK c ctx
              | None -> NOT_FOUND "" ctx
  /// handle command and add result to repository
  let ``BAD_REQUEST_or_`` result (context:CommandContext) ofJson toCommand toJson=fun (ctx) -> monad {
    match Json.getBody ctx with
    | Some body ->
      match ofJson context body with
      | Ok parsed->
        let! res = lift (onCommand(context,toCommand parsed))
        return! result (toJson (res,parsed)) ctx
      | Error err -> return! (BAD_REQUEST err ctx)
    | None -> return! (BAD_REQUEST "Unable to parse JSON" ctx) }

  let ``OK_or_BAD_REQUEST`` context = ``BAD_REQUEST_or_`` Json.OK context
  let ``CREATED_or_BAD_REQUEST`` context = ``BAD_REQUEST_or_`` Json.CREATED context

  let createContact (context:CommandContext) : WebPart=
    POST >=> ``CREATED_or_BAD_REQUEST`` context
              OfJson.contactReq
              AddContact // tocommand
              (snd >> ToJson.contact)

  let createActivity (context:CommandContext) contactId : WebPart=
    POST >=> ``CREATED_or_BAD_REQUEST`` context
              OfJson.activityReq
              (fun activity->AddActivity(ContactId contactId, activity)) // tocommand
              (snd >> ToJson.activity)

  let createComment (context:CommandContext) contactId : WebPart=
    POST >=> ``CREATED_or_BAD_REQUEST`` context
              OfJson.commentReq
              (fun comment->AddComment(ContactId contactId, comment))  // tocommand
              (snd >> ToJson.comment)

  let updateContact (context:CommandContext) contactId : WebPart=
    POST >=> ``OK_or_BAD_REQUEST`` context
              (OfJson.updateContactReq (ContactId contactId))
              UpdateContact // tocommand
              (fun _ -> JNull)

  let updateActivity (context:CommandContext) (contactId,activityId): WebPart=
    POST >=> ``OK_or_BAD_REQUEST`` context
              (OfJson.updateActivityReq (ContactId contactId) (ActivityId activityId))
              UpdateActivity // tocommand
              (fun _ -> JNull)

  let associateContact (context:CommandContext) (contactId,otherContactId): WebPart=
    PUT >=> ``OK_or_BAD_REQUEST`` context
              (OfJson.associateReq (ContactId contactId) (ContactId otherContactId))
              AssociateContactToContact // tocommand
              (fun _ -> JNull)

  WebPart.choose [ path "/" >=> (OK "")
                   authenticated (function
                    | NoSession -> UNAUTHORIZED "Not logged in"
                    | UserLoggedOn user ->
                      let context = { at= time(); userId=user }
                      WebPart.choose [ // contacts
                                       path Paths.contacts >=> overview
                                       path Paths.contacts >=> (createContact context)
                                       pathScan Paths.contact contactDetails
                                       pathScan Paths.contact (updateContact context)
                                       // activities
                                       pathScan Paths.activities getActivities
                                       pathScan Paths.activities (createActivity context)
                                       pathScan Paths.activity getActivity
                                       pathScan Paths.activity (updateActivity context)
                                       // comments
                                       pathScan Paths.comments getComments
                                       pathScan Paths.comments (createComment context)
                                       pathScan Paths.comment getComment
                                       // associate contact
                                       pathScan Paths.associatedContacts getAssociatedContacts
                                       pathScan Paths.associateContact (associateContact context)
                                       ]
                    )]
