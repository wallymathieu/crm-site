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
  let associatedEntitys : Int64Path = "/contacts/%d/contacts"
  let associateEntity : Int64AndInt64Path = "/contacts/%d/contacts/%d"
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
  let comment (EntityId entityId) (comment: Comment) :JsonValue =
    jobj [
      "commentId" .= comment.commentId
      "commentUri" .= (sprintf Paths.comment entityId comment.commentId)
      "comment" .= comment.comment
    ]

  let activity (EntityId entityId) (activity: Activity) :JsonValue =
    jobj [
      "activityId" .= activity.activityId
      "activityUri" .= (sprintf Paths.activity entityId activity.activityId)
      "description" .= activity.description
      "at" .= activity.at
      "tags" .= activity.tags
    ]
  let contactProperties (contact: Entity)=
    let activities = List.map (activity contact.entityId) contact.activities |> JsonValue.ofList
    let comments = List.map (comment contact.entityId) contact.comments |> JsonValue.ofList
    [
      "entityId" .= EntityId.unwrap contact.entityId
      "contactUri" .= (sprintf Paths.contact <| EntityId.unwrap contact.entityId)
      "name" .= contact.name
      "phone" .= contact.phone
      "email" .= contact.email
      "tags" .= contact.tags
      "activities", activities
      "comments", comments
    ]
  let contact (contact: Entity) :JsonValue =
    jobj <| contactProperties contact
  let associatedEntity (contact: Entity, tag) :JsonValue =
    jobj <| ["type" .= tag] @ (contactProperties contact)
module OfJson=
  let contactReq context json : Entity ParseResult =
    let create name phone email tags= {
      entityId = CommandContext.idOf name context |> EntityId
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

  let updateEntityReq (entityId)  _ json:  (EntityId * string option* string option* string option * string list option) ParseResult=
    //name:string * phone:string * email:string
    let create name phone email tags= (entityId, name, phone, email, tags)
    match json with
    | JObject o -> create <!> (o .@? "name") <*> (o .@? "phone") <*> (o .@? "email") <*> (o .@? "tags")
    | x -> Decode.Fail.objExpected x

  let updateActivityReq entityId activityId _ json: (EntityId * ActivityId * string option* DateTime option* string list option) ParseResult =
    let create desc at tags = (entityId, activityId, desc,at,tags)
    match json with
    | JObject o -> create <!> (o .@? "description") <*> (o .@? "at") <*> (o .@? "tags")
    | x -> Decode.Fail.objExpected x

  let associateReq entityId otherEntityId _ json : (EntityId*string*EntityId) ParseResult=
    //AssociateEntityToEntity (from,association,to')
    let create typ  = (entityId, typ, otherEntityId)
    match json with
    | JObject o -> create <!> (o .@ "type")
    | x -> Decode.Fail.objExpected x

let webPart authenticated (repository : IEntityRepository) onCommand (time:unit->DateTime)=
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
                let contactList =repository.GetEntitys() |> List.map ToJson.contact |> JsonValue.ofList
                Json.OK contactList ctx

    let get id=
      GET >=> fun (ctx) ->
                match repository.GetEntity(EntityId id) |> Option.map ToJson.contact with
                | Some c->Json.OK c ctx
                | None -> NOT_FOUND "" ctx

    let create =
      POST >=> ``persist_then_CREATED_or_BAD_REQUEST`` context
                OfJson.contactReq
                AddEntity // tocommand
                (snd >> ToJson.contact)

    let update entityId =
      POST >=> ``persist_then_OK_or_BAD_REQUEST`` context
                (OfJson.updateEntityReq (EntityId entityId))
                UpdateEntity // tocommand
                (fun _ -> JNull)

    [path Paths.contacts >=> overview
     path Paths.contacts >=> create
     pathScan Paths.contact get
     pathScan Paths.contact update]

  let activities context =

    let list id=
      GET >=> fun (ctx) ->
                let entityId = EntityId id
                match repository.GetEntity entityId
                      |> Option.map (fun c->List.map (ToJson.activity entityId) c.activities |> JsonValue.ofList) with
                | Some c->Json.OK c ctx
                | None -> NOT_FOUND "" ctx

    let get (entityId,activityId)=
      GET >=> fun (ctx) ->
                let entityId = EntityId entityId
                match repository.GetEntity entityId
                      |> Option.bind (Entity.activityWithId activityId) |> Option.map (ToJson.activity entityId)with
                | Some c->Json.OK c ctx
                | None -> NOT_FOUND "" ctx

    let create entityId : WebPart<_>=
      let id = EntityId entityId
      POST >=> ``persist_then_CREATED_or_BAD_REQUEST`` context
                OfJson.activityReq
                (fun activity->AddActivity(id, activity)) // tocommand
                (snd >> (ToJson.activity id))

    let update (entityId,activityId): WebPart<_>=
      POST >=> ``persist_then_OK_or_BAD_REQUEST`` context
                (OfJson.updateActivityReq (EntityId entityId) (ActivityId activityId))
                UpdateActivity // tocommand
                (fun _ -> JNull)

    [pathScan Paths.activities list
     pathScan Paths.activities create
     pathScan Paths.activity get
     pathScan Paths.activity update]

  let comments context =
    let list id=
      GET >=> fun (ctx) ->
                let entityId = EntityId id
                match repository.GetEntity entityId
                      |> Option.map (fun c->List.map (ToJson.comment entityId) c.comments |> JsonValue.ofList) with
                | Some c->Json.OK c ctx
                | None -> NOT_FOUND "" ctx

    let get (entityId,commentId)=
      GET >=> fun (ctx) ->
                let entityId = EntityId entityId
                match repository.GetEntity entityId
                      |> Option.bind (Entity.commentWithId commentId) |> Option.map (ToJson.comment entityId) with
                | Some c->Json.OK c ctx
                | None -> NOT_FOUND "" ctx

    let create entityId : WebPart<_>=
      let id = EntityId entityId
      POST >=> ``persist_then_CREATED_or_BAD_REQUEST`` context
                OfJson.commentReq
                (fun comment->AddComment(id, comment))  // tocommand
                (snd >> (ToJson.comment id))

    [pathScan Paths.comments list
     pathScan Paths.comments create
     pathScan Paths.comment get]

  let associations context =
    let associateEntity (entityId,otherEntityId): WebPart<_>=
      PUT >=> ``persist_then_OK_or_BAD_REQUEST`` context
                (OfJson.associateReq (EntityId entityId) (EntityId otherEntityId))
                AssociateEntityToEntity // tocommand
                (fun _ -> JNull)
    let getAssociatedEntitys id=
      GET >=> fun (ctx) ->
                match repository.GetEntityAssociations(EntityId id)
                      |> List.map ToJson.associatedEntity with
                | [] -> NOT_FOUND "" ctx
                | list->Json.OK (JsonValue.ofList list) ctx

    [pathScan Paths.associatedEntitys getAssociatedEntitys
     pathScan Paths.associateEntity associateEntity]

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
