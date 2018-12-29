module Crm.Domain
open System
open FSharpPlus
open System.Security.Cryptography
open System.Text
(*
using (var sha = SHA256.Create())
            {
                var bytes = Encoding.UTF8.GetBytes(input);
                var hash = sha.ComputeHash(bytes);

                return Convert.ToBase64String(hash);
            }
*)
module SHA512=
  let ofList (arr:string list)=
    use sha = SHA512.Create()
    let bytes =
      arr |> Array.ofList
          |> Array.collect Encoding.UTF8.GetBytes
          |> sha.ComputeHash
    BitConverter.ToInt64(bytes, 0)

[<Struct>]
type UserId = UserId of string
with
  override this.ToString()=match this with UserId uId->uId


[<Struct>]
type ContactId = ContactId of int64
with
  override this.ToString()=match this with ContactId cId->sprintf "co%d" cId
module ContactId =
  let unwrap (ContactId cId) = cId

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

type Activity={
  activityId:int64
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
module Contact=
  let activityWithId (id) (c:Contact) = c.activities |> List.tryFind ((=) id << Activity.id)
  let commentWithId (id) (c:Contact) = c.comments |> List.tryFind ((=) id << Comment.id)
type CommandContext={at: DateTime ; userId: UserId}
module CommandContext=
  let at (ctx:CommandContext) = ctx.at
  let toStrList (ctx:CommandContext) = [ctx.at.ToString("O"); string ctx.userId]
  /// Compute an id based on name and when the contact was created
  let idOf (text:string) (ctx:CommandContext) = text:: toStrList ctx |> SHA512.ofList
type CommandError = | ContactNotFound
type CommandResult =
  | ContactAdded
  | ContactModified
type Command =
  | AddActivity of ContactId * Activity
  | UpdateActivity of ContactId * ActivityId * description:string option * at:DateTime option * tags:string list option
  | IgnoreActivity of ContactId * ActivityId
  | CompleteActivity of ContactId * ActivityId
  | AddComment of ContactId * Comment
  | RemoveComment of ContactId * CommentId
  | AddContact of Contact
  | UpdateContact of ContactId * name:string option * phone:string option * email:string option *tags:string list option
  | AssociateContactToContact of from:ContactId * association:string * to':ContactId
  | RemoveContact of ContactId

[<Interface>]
type IContactRepository=
    abstract member GetContact: ContactId->Contact option
    abstract member GetContacts: unit->Contact list
    abstract member GetContactAssociations: ContactId->(Contact*string) list

    abstract member Save: Contact->unit
    abstract member Remove: ContactId->unit
    abstract member Associate: ContactId->string->ContactId->unit

[<AutoOpen>]
module ContactRepository=
  type IContactRepository with
      member self.Handle(command) =
          let notWithActivityId activityId = not << (=) activityId << Activity.id
          let notWithCommentId commentId = not << (=) commentId << Comment.id
          let modified ()= ContactModified

          match command with
          | AddActivity(contactId, activity) ->
            self.GetContact contactId
            |> Option.map (fun c->
              self.Save({ c with activities = activity:: c.activities } ))
            |> Result.ofOption ContactNotFound
            |> Result.map modified
          | UpdateActivity (contactId, (ActivityId activityId), maybeDescription, maybeAt, maybeTags)->
            self.GetContact contactId
            |> Option.map (fun c->
              let maybeUpdateDescription c= match maybeDescription with | Some description -> {c with description=description} | None -> c
              let maybeUpdateAt (a:Activity)= match maybeAt with | Some at -> {a with at=at} | None -> a
              let maybeUpdateTags (a:Activity)= match maybeTags with | Some tags -> {a with tags=tags} | None -> a
              let update = maybeUpdateDescription>> maybeUpdateAt >> maybeUpdateTags
              let replaceActivity (a:Activity) = if a.activityId = activityId then update a else a
              self.Save({ c with activities = List.map replaceActivity c.activities } ) )
            |> Result.ofOption ContactNotFound
            |> Result.map modified
          | IgnoreActivity (contactId, (ActivityId activityId)) ->
            self.GetContact contactId
            |> Option.map (fun c->
              let contact = { c with activities = List.filter (notWithActivityId activityId) c.activities }
              self.Save contact)
            |> Result.ofOption ContactNotFound
            |> Result.map modified
          | CompleteActivity (contactId, (ActivityId activityId)) ->
            self.GetContact contactId
            |> Option.map (fun c->
              let contact = { c with activities = List.filter (notWithActivityId activityId) c.activities }
              self.Save contact)
            |> Result.ofOption ContactNotFound
            |> Result.map modified
          | AddComment (contactId, comment) ->
            self.GetContact contactId
            |> Option.map (fun c->
              let contact = { c with comments = comment :: c.comments }
              self.Save contact)
            |> Result.ofOption ContactNotFound
            |> Result.map modified
          | RemoveComment (contactId, (CommentId commentId)) ->
            self.GetContact contactId
            |> Option.map (fun c->
              let contact = { c with comments = List.filter (notWithCommentId commentId) c.comments }
              self.Save contact)
            |> Result.ofOption ContactNotFound
            |> Result.map modified
          | AddContact contact ->
            self.Save contact
            Ok ContactAdded
          | UpdateContact (contactId, maybeName, maybePhone, maybeEmail, maybeTags) ->
            let maybeUpdateName c= match maybeName with | Some name -> {c with name=name} | None -> c
            let maybeUpdatePhone c= match maybePhone with | Some phone -> {c with phone=phone} | None -> c
            let maybeUpdateEmail c= match maybeEmail with | Some email -> {c with email=email} | None -> c
            let maybeUpdateTags (c:Contact)= match maybeTags with | Some tags -> {c with tags=tags} | None -> c
            let update = maybeUpdateName>> maybeUpdatePhone >> maybeUpdateEmail >> maybeUpdateTags
            self.GetContact contactId
            |> Option.map (self.Save << update)
            |> Result.ofOption ContactNotFound
            |> Result.map modified
          | AssociateContactToContact (from,association,to')->
            self.Associate from association to'
            Ok ContactModified
          | RemoveContact contactId->
            self.Remove contactId
            Ok ContactModified

/// Simple in memory repository
type ContactRepository()=
  let mutable contacts = Map.empty
  let mutable contactAssociations = Map.empty

  interface IContactRepository with
    member __.GetContact (ContactId id) = Map.tryFind id contacts
    member __.GetContacts ()= Map.values contacts |> Seq.toList
    member __.Save (c:Contact) =
      let cId = ContactId.unwrap c.contactId
      contacts <- Map.add cId c contacts
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

