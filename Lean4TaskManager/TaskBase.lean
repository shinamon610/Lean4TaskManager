import Std
import Lean.Util.FoldConsts
import Lean4MyLib.MyDate
import Lean4MyLib.DirectedGraph
import Lean
open Std
open Std.Time
open Lean

class Doneable (A:Type) where
  isDone: A->Bool

structure Operator where
  name:String
deriving Repr,Inhabited,BEq,Hashable,ToJson

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
  «終了日»:Option ZonedDateTime
  details:String
  result:String

deriving Inhabited

instance [ToJson Status][ToJson Tag]: ToJson (TaskBase Status Tag) where
  toJson t:=
    Json.mkObj [
      ("name", toJson t.name),
      ("status", toJson t.status),
      ("assign", toJson t.assign),
      ("tags", toJson t.tags),
      ("開始予定日", toJson t.«開始予定日»),
      ("終了予定日", toJson t.«終了予定日»),
      ("details", toJson t.details),
      ("result", toJson t.result),
    ]


instance :Repr (TaskBase  Status Tag) where
  reprPrec tb _ := tb.name

instance :BEq (TaskBase Status Tag) where
  beq t1 t2:=t1.name==t2.name

instance :Hashable (TaskBase Status Tag) where
  hash t:=hash t.name

instance : ToString (TaskBase Status Tag) where
  toString mytask := s! "{mytask.name}"

def TaskBase.new [Inhabited Status](name:String) (tags:List Tag:=[])  (operator:Option Operator:=none) (status:Status:= default) (links:List KnowledgeLink:=[]) («開始予定日»:Option ZonedDateTime:=none) («終了予定日»:Option ZonedDateTime:=none) («終了日»:Option ZonedDateTime:=none) (details:="") (result:="") :TaskBase Status Tag :=
  {name,status:=status, assign:=operator,tags,links, «開始予定日»,«終了予定日», «終了日», details, result}

def inner_isAllChildrenValid [Doneable A] (dag:PackedDAG A) (target:Fin dag.1) : Bool :=
  match dag with
  | ⟨n, sdag⟩ =>
    if Doneable.isDone (sdag.label target)
    then true
    else (List.finRange n).all (fun fin =>
      let kidsAsFinN : List (Fin n) := (sdag.kids fin).map (DAG.coeChild fin) -- そのままだとtargetと比較できないから"持ち上げる"必要があるらしい。意味不明
      let hasTarget:=kidsAsFinN.contains target
      (!hasTarget || !Doneable.isDone (sdag.label fin))
      )

def isAllChildrenValidDAG [Doneable A] (dag:PackedDAG A):Bool :=
  (List.finRange dag.1).all (fun fin=>inner_isAllChildrenValid dag fin)

def printDoneLog [Doneable Status][ToJson (TaskBase Status Tag)] [Inhabited (TaskBase Status Tag)] (dag:PackedDAG (TaskBase Status Tag)):IO Unit:=do
  let current <- now
  let filename:String := (current.toISO8601String.takeWhile  (fun x=> x != 'T')) ++ ".json"
  let fd :=  dag.WithFilterOf (fun (t, _) =>
    match t.«終了日» with
    | none=>Doneable.isDone t.status
    | some d=> (Doneable.isDone t.status && is_same_date current d))
  IO.FS.writeFile filename (toJson fd.compress).pretty

partial def pritnTsv (dag : PackedDAG (TaskBase Status Tag)) : IO Unit := do
  let output :=
    match dag with
    | ⟨n, sdag⟩ =>
      -- すべてのノード
      let all : List (Fin n) := List.finRange n

      -- 全ての子ノード
      let allChildren : List (Fin n) :=
        all.flatMap (fun i => (sdag.kids i).map (DAG.coeChild i))

      -- 親を持たないノードを root とみなす
      let roots : List (Fin n) :=
        all.filter (fun i => ¬ allChildren.contains i)

      -- 深さに応じてタブを付けて name を 1 行出力する DFS
      let rec dfs (i : Fin n) (depth : Nat) (visited : List (Fin n)) : List String :=
        if visited.contains i then
          []
        else
          let indent := String.join (List.replicate depth "\t")
          let line   := indent ++ "\"" ++ (sdag.label i).name ++ "\""
          let children : List (Fin n) := (sdag.kids i).map (DAG.coeChild i)
          let childLines :=
            children.flatMap (fun j => dfs j (depth + 1) (i :: visited))
          line :: childLines

      let lines := roots.flatMap (fun r => dfs r 0 [])
      String.intercalate "\n" lines

  IO.println output
