import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Data.Char (ord)
import Data.List (minimumBy, sortBy)

type Node = Char
type Time = Int
type Job = (Node, Time)
type Edge = (Node, Node)
data Graph = Graph {nodes::(Set Node), edges::(Set Edge)} 
    deriving (Show, Eq, Ord)

newGraph :: Graph
newGraph = Graph (Set.empty) (Set.empty)

parseIntoGraph :: Graph -> [String] -> Graph
parseIntoGraph g t = (Graph n e)
    where 
        (req, stp) = (head (t!!1), head (t!!7))
        n = Set.insert req $ Set.insert stp $ nodes g
        e = Set.insert (req, stp) $ edges g

requirements :: Node -> Graph -> Set Node
requirements c g = Set.map (fst) $ Set.filter ((c==) . snd) (edges g)

fulfill :: Node -> Graph -> Graph
fulfill r g = Graph (Set.delete r $ nodes g) (snd $ Set.partition ((==r) . fst) $ edges g)

startable :: Graph -> Set Node
startable g = Set.filter (Set.null . flip requirements g) $ nodes g

partOne :: Graph -> String
partOne g
    | Set.null $ nodes g = []
    | otherwise = (m : (partOne $ fulfill m g))
    where m = Set.findMin $ startable g

duration :: Node -> Time
duration = ((32+60-96) +) . ord

partTwo :: Graph -> [Job] -> Time
partTwo graph jobs
    | work && idle = distribute
    | work = finishEndingJob
    | otherwise = maximum finish
    where
        work = Set.size (nodes graph) > 0
        idle = length jobs < 5 && Set.size (startable graph) > 0
        distribute = partTwo graph{nodes=(Set.delete newJobNode (nodes graph))} (newJob:jobs)
        newJobNode = Set.findMin $ startable graph
        newJobTime = duration newJobNode
        newJob = (newJobNode, newJobTime)
        finishEndingJob = endingJobTime + (partTwo (fulfill endingJobNode graph) agedJobs)
        (endingJobNode, endingJobTime) = minimumBy (\(_, a) (_, b) -> compare a b) jobs
        agedJobs = [(jn, jt-endingJobTime) | (jn, jt)<-jobs, jn /= endingJobNode]
        finish = map snd (sortBy (\(_, a) (_, b) -> compare b a) jobs)

main :: IO ()
main = do
    input <- getContents
    let graph = foldl parseIntoGraph newGraph (map words $ lines input)
    print $ partOne graph
    let jobs = []
    print $ partTwo graph jobs
