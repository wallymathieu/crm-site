module Crm.Domain
open System
open FSharpPlus
open Crm.Security.Cryptography
[<Struct>]
type UserId = UserId of string
with
  override this.ToString()=match this with UserId uId->uId


[<Struct>]
type EntityId = EntityId of string * int64
with
  override this.ToString()=match this with EntityId (typ, cId)->sprintf "%s%d" typ cId
module EntityId =
  let unwrap (EntityId (typ, cId)) = (typ,cId)

[<Struct>]
type CommentId = CommentId of int64
with
  override this.ToString()=match this with CommentId commentId->sprintf "cm%d" commentId

[<Struct>]
type ActivityId = ActivityId of int64
with
  override this.ToString()=match this with ActivityId accountId->sprintf "ac%d" accountId

type Comment={
  commentId:int64
  comment:string
}
module Comment=
  let id (c:Comment)=c.commentId
/// Things done or to do
type Activity={
  activityId:int64
  at:DateTime
  description:string
  tags:string list
}
module Activity=
  let id (a:Activity)=a.activityId
/// A contact, project or a company
type Entity={
  entityId:EntityId
  name:string
  phone:string; email:string;
  tags:string list
  comments: Comment list
  activities: Activity list
  uris: Uri list
}
module Entity=
  let activityWithId (id) (c:Entity) = c.activities |> List.tryFind ((=) id << Activity.id)
  let commentWithId (id) (c:Entity) = c.comments |> List.tryFind ((=) id << Comment.id)
type CommandContext={at: DateTime ; userId: UserId}
module CommandContext=
  let at (ctx:CommandContext) = ctx.at
  let toStrList (ctx:CommandContext) = [ctx.at.ToString("O"); string ctx.userId]
  /// Compute an id based on name and when the Entity was created
  let idOf (text:string) (ctx:CommandContext) = text:: toStrList ctx |> SHA512.ofList
type CommandError = | EntityNotFound
type CommandResult =
  | EntityAdded
  | EntityModified
type Command =
  | AddActivity of EntityId * Activity
  | UpdateActivity of EntityId * ActivityId * description:string option * at:DateTime option * tags:string list option
  | IgnoreActivity of EntityId * ActivityId
  | CompleteActivity of EntityId * ActivityId
  | AddComment of EntityId * Comment
  | UpdateComment of EntityId * CommentId * comment:string option
  | RemoveComment of EntityId * CommentId
  | AddEntity of Entity
  | UpdateEntity of EntityId * name:string option * phone:string option * email:string option *tags:string list option
  | AssociateEntityToEntity of from:EntityId * association:string * to':EntityId
  | RemoveEntity of EntityId

[<Interface>]
type IEntityRepository=
    abstract member GetEntity: EntityId->Entity option
    abstract member GetEntitys: unit->Entity list
    abstract member GetEntityAssociations: EntityId->(Entity*string) list

    abstract member Save: Entity->unit
    abstract member Remove: EntityId->unit
    abstract member Associate: EntityId->string->EntityId->unit

module Result=
  let ofOption err = function | Some ok -> Ok ok | None->Error err

[<AutoOpen>]
module EntitytRepository=
  type IEntityRepository with
      member self.Handle(command) =
          let notWithActivityId activityId = not << (=) activityId << Activity.id
          let notWithCommentId commentId = not << (=) commentId << Comment.id
          let modified ()= EntityModified

          match command with
          | AddActivity(entityId, activity) ->
            self.GetEntity entityId
            |> Option.map (fun c->
              self.Save({ c with activities = activity:: c.activities } ))
            |> Result.ofOption EntityNotFound
            |> Result.map modified
          | UpdateActivity (entityId, (ActivityId activityId), maybeDescription, maybeAt, maybeTags)->
            self.GetEntity entityId
            |> Option.map (fun c->
              let maybeUpdateDescription c= match maybeDescription with | Some description -> {c with description=description} | None -> c
              let maybeUpdateAt (a:Activity)= match maybeAt with | Some at -> {a with at=at} | None -> a
              let maybeUpdateTags (a:Activity)= match maybeTags with | Some tags -> {a with tags=tags} | None -> a
              let update = maybeUpdateDescription>> maybeUpdateAt >> maybeUpdateTags
              let replaceActivity (a:Activity) = if a.activityId = activityId then update a else a
              self.Save({ c with activities = List.map replaceActivity c.activities } ) )
            |> Result.ofOption EntityNotFound
            |> Result.map modified
          | IgnoreActivity (entityId, (ActivityId activityId)) ->
            self.GetEntity entityId
            |> Option.map (fun c->
              let entity = { c with activities = List.filter (notWithActivityId activityId) c.activities }
              self.Save entity)
            |> Result.ofOption EntityNotFound
            |> Result.map modified
          | CompleteActivity (entityId, (ActivityId activityId)) ->
            self.GetEntity entityId
            |> Option.map (fun c->
              let entity = { c with activities = List.filter (notWithActivityId activityId) c.activities }
              self.Save entity)
            |> Result.ofOption EntityNotFound
            |> Result.map modified
          | AddComment (entityId, comment) ->
            self.GetEntity entityId
            |> Option.map (fun c->
              let entity = { c with comments = comment :: c.comments }
              self.Save entity)
            |> Result.ofOption EntityNotFound
            |> Result.map modified
          | UpdateComment (entityId, (CommentId commentId), maybeComment)->
            self.GetEntity entityId
            |> Option.map (fun c->
              let maybeUpdateComment c= match maybeComment with | Some comment -> {c with comment=comment} | None -> c
              let update = maybeUpdateComment
              let replaceComment (c:Comment) = if c.commentId = commentId then update c else c
              self.Save({ c with comments = List.map replaceComment c.comments } ) )
            |> Result.ofOption EntityNotFound
            |> Result.map modified
          | RemoveComment (entityId, (CommentId commentId)) ->
            self.GetEntity entityId
            |> Option.map (fun c->
              let entity = { c with comments = List.filter (notWithCommentId commentId) c.comments }
              self.Save entity)
            |> Result.ofOption EntityNotFound
            |> Result.map modified
          | AddEntity entity ->
            self.Save entity
            Ok EntityAdded
          | UpdateEntity (entityId, maybeName, maybePhone, maybeEmail, maybeTags) ->
            let maybeUpdateName c= match maybeName with | Some name -> {c with name=name} | None -> c
            let maybeUpdatePhone c= match maybePhone with | Some phone -> {c with phone=phone} | None -> c
            let maybeUpdateEmail c= match maybeEmail with | Some email -> {c with email=email} | None -> c
            let maybeUpdateTags (c:Entity)= match maybeTags with | Some tags -> {c with tags=tags} | None -> c
            let update = maybeUpdateName>> maybeUpdatePhone >> maybeUpdateEmail >> maybeUpdateTags
            self.GetEntity entityId
            |> Option.map (self.Save << update)
            |> Result.ofOption EntityNotFound
            |> Result.map modified
          | AssociateEntityToEntity (from,association,to')->
            self.Associate from association to'
            Ok EntityModified
          | RemoveEntity entityId->
            self.Remove entityId
            Ok EntityModified

/// Simple in memory repository
type EntityRepository()=
  let mutable entities = Map.empty
  let mutable entityAssociations = Map.empty

  interface IEntityRepository with
    member __.GetEntity (EntityId (typ,id)) = Map.tryFind (typ,id) entities
    member __.GetEntitys ()= Map.values entities |> Seq.toList
    member __.Save (c:Entity) =
      let entityId = EntityId.unwrap c.entityId
      entities <- Map.add entityId c entities
    member __.Remove entityId =
      let entityId = EntityId.unwrap entityId
      entities <- Map.remove entityId entities
      entityAssociations <- Map.remove entityId entityAssociations
    member __.Associate from (association:string) to'
      =
        let from' = EntityId.unwrap from
        let to'' = EntityId.unwrap to'
        let next = Map.tryFind from' entityAssociations |> Option.defaultValue Map.empty
                   |> Map.add to'' association
        entityAssociations <- Map.add from' next entityAssociations
    member __.GetEntityAssociations from
      =
        let from' = EntityId.unwrap from
        match Map.tryFind from' entityAssociations with
        | Some map-> Map.toList map
                     |> List.choose (fun (entityId,association)->
                                      Map.tryFind entityId entities
                                      |> Option.map (fun entity->entity,association))
        | None->[]

