import Std
import Lean.Util.FoldConsts
import Tree.Tree

open Std
open Std.Time
open Tree
namespace TaskBase

class Doneable (A:Type) where
  isDone: A->Bool

structure Operator where
  name:String
deriving Repr,Inhabited,BEq,Hashable

structure Progress where
  current:Float
  all:Float
deriving Repr,Inhabited,BEq
instance :Hashable Progress where
  hash progress:=hash (progress.current+progress.all).toInt32

structure TaskBase (Status Tag:Type)  where
  name:String
  status:Status
  assign:Option Operator
  tags:List Tag
  «開始予定日»:Option ZonedDateTime
  «終了予定日»:Option ZonedDateTime
  progress:Option Progress
deriving Inhabited

instance :Repr (TaskBase Status Tag) where
  reprPrec tb _ := tb.name

instance :BEq (TaskBase Status Tag) where
  beq t1 t2:=t1.name==t2.name

instance :Hashable (TaskBase Status Tag) where
  hash t:=hash t.name

instance : ToString (TaskBase Status Tag) where
  toString mytask := s! "{mytask.name}"

def exec (stt:StateM S A) (init:S):S:= (stt.run init).snd

def TaskBase.new(name:String) (tags:List Tag)  (operator:Option Operator:=none) (status:Option Status:= none) («開始予定日»:Option ZonedDateTime:=none) («終了予定日»:Option ZonedDateTime:=none)  (progress:Option Progress:=none)[Inhabited Status]:TaskBase Status Tag :=
  {name,status:=(status.getD default), assign:=operator,tags,«開始予定日»,«終了予定日»,progress}

def toList {A:Type}(root:Tree A): List A:=
  match root with
  | .Node parent children => parent::(children.flatMap toList)

def isAllChildrenDone [Doneable Status]: Tree (TaskBase Status Tag) → Bool
  | .Node task children =>
    let allChildrenDone :=
      children.all (fun child => Doneable.isDone (top child).status)
    let childrenValid := children.map isAllChildrenDone |>.all id --ここを単純にallにするとterminationが証明できない
    if Doneable.isDone task.status then
      allChildrenDone && childrenValid
    else
      childrenValid

def root_tree [Inhabited Status]:Tree (TaskBase Status Tag) :=Tree.Node (.new "root" []) []

structure DailyTask (Status Tag:Type) where
  tt:Tree (TaskBase Status Tag)
  «最後に完了した日»:ZonedDateTime

instance : ToString (DailyTask Status Tag) where
  toString daily :=
    s! "{daily.tt}"

def root_daily (now:ZonedDateTime) [Inhabited Status]:Tree (DailyTask Status Tag) :=Tree.Node {tt:= root_tree, «最後に完了した日»:= now } []

def show_task (allTasks:Tree (TaskBase Status Tag))(condition:(TaskBase Status Tag)->Bool) [Inhabited Status]:Tree (TaskBase Status Tag):=
  let res:=find condition allTasks
  res.get!
