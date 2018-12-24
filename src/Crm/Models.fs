module Models
open System
[<Struct>]
type UserId = UserId of string
with
  override this.ToString()=match this with UserId uId->uId

type User={
  userId: UserId
  name:string; phone:string; email:string;
  tags:string list  
}
[<Struct>]
type CommentId = CommentId of string
with
  override this.ToString()=match this with CommentId cId->cId

type Comment={
  commentId:CommentId
  createdAt:DateTime
  comment:string
  byUser: UserId
  tags:string list 
}

[<Struct>]
type ActivityId = ActivityId of string
with
  override this.ToString()=match this with ActivityId aId->aId

type Activity={
  activityId:ActivityId
  createdAt:DateTime
  at:DateTime
  comment:string
  byUser: UserId
  tags:string list 
}

[<Struct>]
type ContactId = ContactId of string
with
  override this.ToString()=match this with ContactId cId->cId
type Contact={
  contactId:ContactId
  name:string; phone:string; email:string;
  tags:string list 
  comments: Comment list
  activities: Activity list
}

[<Struct>]
type AccountId = AccountId of string
with
  override this.ToString()=match this with AccountId aId->aId

///  a qualified Sales Prospect, Customer, Supplier or Re-seller 
type Account={ 
  accountId: AccountId
  assignedTo: UserId list;
  name:string; phone:string; email:string;
  rating:int;
  tags:string list 
  contacts:Contact list
  comments: Comment list
  activities: Activity list
}


[<Struct>]
type TaskId = TaskId of string
with
  override this.ToString()=match this with TaskId tId->tId

type Task={
  taskId: TaskId
  assignedTo: UserId;
  description:string
  createdAt:DateTime
  completedAt:DateTime option
  ignoredAt:DateTime option
}
