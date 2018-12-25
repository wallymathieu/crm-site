module Models
open System
open FSharpPlus

[<Struct>]
type UserId = UserId of string
with
  override this.ToString()=match this with UserId uId->uId


[<Struct>]
type ContactId = ContactId of int
with
  override this.ToString()=match this with ContactId cId->sprintf "co%d" cId
module ContactId =
  let unwrap (ContactId cId) = cId
[<Struct>]
type CommentId = CommentId of ContactId * int
with
  override this.ToString()=match this with CommentId (contactId,commentId)->sprintf "%O-cm%d" contactId commentId

[<Struct>]
type ActivityId = ActivityId of ContactId * int
with
  override this.ToString()=match this with ActivityId (contactId,accountId)->sprintf "%O-ac%d" contactId accountId

type Comment={
  commentId:int
  comment:string
}
module Comment=
  let id (c:Comment)=c.commentId

type Activity={
  activityId:int
  at:DateTime
  description:string
  tags:string list
}
module Activity=
  let id (a:Activity)=a.activityId

type Contact={
  contactId:ContactId
  name:string
  phone:string; email:string;
  tags:string list
  comments: Comment list
  activities: Activity list
}

type CommandContext={at: DateTime ; userId: UserId}
type CommandError = | ContactNotFound

type Command =
  | AddActivity of ContactId * at:DateTime*comment:string*tags:string list
  | SetActivityTags of ActivityId * tags:string list
  | IgnoreActivity of ActivityId
  | CompleteActivity of ActivityId
  | AddContactComment of ContactId * comment:string
  | RemoveComment of CommentId
  | AddContact of name:string * phone:string * email:string * tags:string list
  | UpdateContact of ContactId * name:string * phone:string * email:string
  | SetContactTags of ContactId * tags:string list
  | AssociateContactToContact of from:ContactId * association:string * to':ContactId
  | RemoveContact of ContactId

type IContactRepository=
    abstract member GetContact: ContactId->Contact option
    abstract member GetContacts: unit->Contact list
    abstract member GetContactAssociations: ContactId->(Contact*string) list

    abstract member Save: Contact->unit
    abstract member Remove: ContactId->unit
    abstract member Associate: ContactId->string->ContactId->unit
    abstract member NextContactId: unit -> int
    abstract member NextCommentId: unit -> int
    abstract member NextActivityId: unit -> int

[<AutoOpen>]
module ContactRepository=
  type IContactRepository with
      member self.Handle(command) =
          let notWithActivityId activityId = not << (=) activityId << Activity.id
          let notWithCommentId commentId = not << (=) commentId << Comment.id

          match command with
          | AddActivity(contactId, time, comment, tags) ->
            self.GetContact contactId
            |> Option.map (fun c->
              let activity = { activityId=self.NextCommentId(); description=comment; at =time; tags=tags }
              self.Save({ c with activities = activity:: c.activities } ))
            |> Result.ofOption ContactNotFound
          | SetActivityTags (ActivityId(contactId, activityId),tags)->
            self.GetContact contactId
            |> Option.map (fun c->
              let replaceActivity (a:Activity) = if a.activityId = activityId then { a with tags=tags} else a
              self.Save({ c with activities = List.map replaceActivity c.activities } ) )
            |> Result.ofOption ContactNotFound
          | IgnoreActivity (ActivityId(contactId, activityId)) ->
            self.GetContact contactId
            |> Option.map (fun c->
              let contact = { c with activities = List.filter (notWithActivityId activityId) c.activities }
              self.Save contact)
            |> Result.ofOption ContactNotFound
          | CompleteActivity (ActivityId(contactId, activityId)) ->
            self.GetContact contactId
            |> Option.map (fun c->
              let contact = { c with activities = List.filter (notWithActivityId activityId) c.activities }
              self.Save contact)
            |> Result.ofOption ContactNotFound
          | AddContactComment (contactId, comment) ->
            self.GetContact contactId
            |> Option.map (fun c->
              let comment = { commentId = self.NextCommentId(); comment=comment }
              let contact = { c with comments = comment :: c.comments }
              self.Save contact)
            |> Result.ofOption ContactNotFound
          | RemoveComment (CommentId(contactId, commentId) ) ->
            self.GetContact contactId
            |> Option.map (fun c->
              let contact = { c with comments = List.filter (notWithCommentId commentId) c.comments }
              self.Save contact)
            |> Result.ofOption ContactNotFound
          | AddContact (name, phone, email, tags) ->
            let contact ={contactId=self.NextContactId() |> ContactId;
                        name=name; phone=phone; email=email; tags=tags; comments=[]; activities=[]}
            self.Save contact
            Ok ()
          | UpdateContact (contactId, name, phone, email) ->
            self.GetContact contactId
            |> Option.map (fun c-> self.Save {c with name=name; phone=phone; email=email})
            |> Result.ofOption ContactNotFound
          | SetContactTags (contactId , tags)->
            self.GetContact contactId
            |> Option.map (fun c-> self.Save {c with tags = tags})
            |> Result.ofOption ContactNotFound
          | AssociateContactToContact (from,association,to')->
            self.Associate from association to'
            Ok ()
          | RemoveContact contactId->
            self.Remove contactId
            Ok ()

/// small helper class in order to observe values and store the maximum value
type ThreadSafeMax<'t when 't :comparison>(initial:'t) =
  let monitor = Object()
  let mutable maximum = initial
  /// get the current maximum
  member __.Value with get ()= maximum
  /// observe a value, store the value if it's greater than the current value
  member __.Observe value =
    lock monitor (fun ()-> maximum <- max maximum value)
    value
/// Simple in memory repository
type ContactRepository()=
  let mutable contacts = Map.empty
  let mutable contactAssociations = Map.empty
  let contactId = ThreadSafeMax 0
  let activityId = ThreadSafeMax 0
  let commentId = ThreadSafeMax 0

  interface IContactRepository with
    member __.GetContact (ContactId id) = Map.tryFind id contacts
    member __.GetContacts ()= Map.values contacts |> Seq.toList
    member __.NextContactId ()= 1+contactId.Value
    member __.NextActivityId ()= 1+activityId.Value
    member __.NextCommentId ()= 1+commentId.Value
    member __.Save (c:Contact) =
      let id = contactId.Observe <| ContactId.unwrap c.contactId
      contacts <- Map.add id c contacts
    member __.Remove (ContactId contactId) =
      contacts <- Map.remove contactId contacts
      contactAssociations <- Map.remove contactId contactAssociations
    member __.Associate (ContactId from) (association:string) (ContactId to')
      =
        let next = Map.tryFind from contactAssociations |> Option.defaultValue Map.empty
                   |> Map.add to' association
        contactAssociations <- Map.add from next contactAssociations
    member __.GetContactAssociations (ContactId from)
      =
        match Map.tryFind from contactAssociations with
        | Some map-> Map.toList map
                     |> List.choose (fun (contactId,association)->
                                      Map.tryFind contactId contacts
                                      |> Option.map (fun contact->contact,association))
        | None->[]

