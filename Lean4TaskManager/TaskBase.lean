import Std
import Lean.Util.FoldConsts
import Lean4MyLib.Tree
import Lean4MyLib.MyDate

open Std
open Std.Time
open Tree
namespace TaskBase

class Doneable (A:Type) where
  isDone: A->Bool

structure Operator where
  name:String
deriving Repr,Inhabited,BEq,Hashable

inductive KnowledgeLink
| Link : Option String->KnowledgeLink

structure TaskBase (Status Tag:Type)  where
  name:String
  status:Status
  assign:Option Operator
  tags:List Tag
  links: List KnowledgeLink
  «開始予定日»:Option ZonedDateTime
  «終了予定日»:Option ZonedDateTime
deriving Inhabited

instance :Repr (TaskBase Status Tag) where
  reprPrec tb _ := tb.name

instance :BEq (TaskBase Status Tag) where
  beq t1 t2:=t1.name==t2.name

instance :Hashable (TaskBase Status Tag) where
  hash t:=hash t.name

instance : ToString (TaskBase Status Tag) where
  toString mytask := s! "{mytask.name}"

def TaskBase.new [Inhabited Status](name:String) (tags:List Tag)  (operator:Option Operator:=none) (status:Status:= default) (links:List KnowledgeLink:=[]) («開始予定日»:Option ZonedDateTime:=none) («終了予定日»:Option ZonedDateTime:=none) :TaskBase Status Tag :=
  {name,status:=status, assign:=operator,tags,links, «開始予定日»,«終了予定日»}

def toList {A:Type}(root:Tree A): List A:=
  match root with
  | .Node parent children => parent::(children.flatMap toList)

def isAllChildrenDone [Doneable Status]: Tree (TaskBase Status Tag) -> Bool
  | .Node _ children => children.all (fun child => Doneable.isDone (top child).status)

def isAllChildrenValid [Doneable Status]: Tree (TaskBase Status Tag) → Bool
  | .Node task children =>
    let allChildrenDone :=
      children.all (fun child => Doneable.isDone (top child).status)
    let childrenValid := children.map isAllChildrenValid |>.all id --ここを単純にallにするとterminationが証明できない
    if Doneable.isDone task.status then
      allChildrenDone && childrenValid
    else
      childrenValid

def root [Inhabited Status]:Tree (TaskBase Status Tag) :=Tree.Node (.new "root" []) []

structure DailyTask (Status Tag:Type) where
  tt:Tree (TaskBase Status Tag)
  «最後に完了した日»:ZonedDateTime

instance : ToString (DailyTask Status Tag) where
  toString daily :=
    s! "{daily.tt}"

def root_daily (now:ZonedDateTime) [Inhabited Status]:Tree (DailyTask Status Tag) :=Tree.Node {tt:= root, «最後に完了した日»:= now } []

def show_task (allTasks:Tree (TaskBase Status Tag))(condition:(TaskBase Status Tag)->Bool) [Inhabited Status]:Tree (TaskBase Status Tag):=
  let res:=find condition allTasks
  res.get!

def «今日かそれ以前に開始» (task:TaskBase S Tag) :IO Bool := do
  let now <- now
  return (task.«開始予定日» <&> fun date => decide (date <= now)).getD false

def «n日後に開始» (task:TaskBase S Tag) (n:Day.Offset) :IO Bool := do
  let now <- now
  return (task.«開始予定日» <&> fun date => decide (date == (now.addDays n))).getD false

def «今日かそれ以前に終了» (task:TaskBase S Tag) :IO Bool := do
  let now <- now
  return (task.«終了予定日» <&> fun date => decide (date <= now)).getD false

def «n日後に終了» (task:TaskBase S Tag) (n:Day.Offset) :IO Bool := do
  let now <- now
  return (task.«終了予定日» <&> fun date => decide (date == (now.addDays n))).getD false
