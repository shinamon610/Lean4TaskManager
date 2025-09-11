import Std
import Lean.Util.FoldConsts
import Lean4MyLib.MyDate
import Lean4MyLib.DAG
import Lean
open Std
open Std.Time
open Lean

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
  details:String
  result:String

deriving Inhabited

instance :Repr (TaskBase  Status Tag) where
  reprPrec tb _ := tb.name

instance :BEq (TaskBase Status Tag) where
  beq t1 t2:=t1.name==t2.name

instance :Hashable (TaskBase Status Tag) where
  hash t:=hash t.name

instance : ToString (TaskBase Status Tag) where
  toString mytask := s! "{mytask.name}"

def TaskBase.new [Inhabited Status](name:String) (tags:List Tag)  (operator:Option Operator:=none) (status:Status:= default) (links:List KnowledgeLink:=[]) («開始予定日»:Option ZonedDateTime:=none) («終了予定日»:Option ZonedDateTime:=none) (details:="") (result:="") :TaskBase Status Tag :=
  {name,status:=status, assign:=operator,tags,links, «開始予定日»,«終了予定日», details, result}

def inner_isAllChildrenValid [Doneable Status] (dag:DAG (TaskBase Status Tag)) (target:Fin dag.1) : Bool :=
  match dag with
  | ⟨n, sdag⟩ =>
    if Doneable.isDone (sdag.label target).status
    then true
    else (List.finRange n).all (fun fin =>
      let kidsAsFinN : List (Fin n) := (sdag.children fin).map (SDAG.coeChild fin) -- そのままだとtargetと比較できないから"持ち上げる"必要があるらしい。意味不明
      let hasTarget:=kidsAsFinN.contains target
      (!hasTarget || !Doneable.isDone (sdag.label fin).status)
      )

def isAllChildrenValidDAG [Doneable Status] (dag:DAG (TaskBase Status Tag)):Bool :=
  (List.finRange dag.1).all (fun fin=>inner_isAllChildrenValid dag fin)

def printDoneLog [Doneable Status][ToJson (TaskBase Status Tag)](dag:DAG (TaskBase Status Tag)):IO Unit:=do
  let current <- now
  let filename:String := (current.toISO8601String.takeWhile  (fun x=> x != 'T')) ++ ".json"
  let fd := DAGWithFilter.of dag (fun (t, _) => Doneable.isDone t.status)
  IO.FS.writeFile filename (toJsonByLabel fd).pretty
