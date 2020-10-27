namespace MentorMatching

open Flips
open Flips.Types
open Flips.SliceMap
open MentorMatching.Types


module internal Set =
    
    let combinations (aSet:'a Set) (bSet:'b Set) =
        aSet
        |> Seq.collect (fun a -> bSet |> Seq.map (fun b -> a, b))
        |> Set.ofSeq


module internal Map =

    let tryOrDefault d k m =
        match Map.tryFind k m with
        | Some v -> v
        | None -> d


module Model =

    let private getSkills (mentees:Mentee seq) (mentors:Mentor seq) =
        let menteeSkills =
            mentees
            |> Seq.collect (fun x -> x.Skills)
            |> Set.ofSeq

        let mentorSkills =
            mentors
            |> Seq.collect (fun x -> x.Skills)
            |> Set.ofSeq

        Set.intersect menteeSkills mentorSkills
    

    let private getPeriods (mentees:Mentee seq) (mentors:Mentor seq) =
        let menteePeriods =
            mentees
            |> Seq.collect (fun x -> x.Periods)
            |> Set.ofSeq

        let mentorPeriods =
            mentors
            |> Seq.collect (fun x -> x.Periods)
            |> Set.ofSeq

        Set.intersect menteePeriods mentorPeriods


    let private getSkillPeriodToMentorsMap (mentors:Mentor seq) =
        mentors
        |> Seq.map (fun mentor -> mentor, Set.combinations mentor.Periods mentor.Skills)
        |> Seq.collect (fun (mentor, skillPeriods) -> skillPeriods |> Seq.map (fun sp -> sp, mentor))
        |> Seq.groupBy fst
        |> Seq.map (fun (skillPeriod, group) ->  skillPeriod, group |> Seq.map snd |> Set.ofSeq)
        |> Map.ofSeq


    let private getSkillPeriodToMenteesMap (mentees:Mentee seq) =
        mentees
        |> Seq.map (fun mentee -> mentee, Set.combinations mentee.Periods mentee.Skills)
        |> Seq.collect (fun (mentee, skillPeriods) -> skillPeriods |> Seq.map (fun sp -> sp, mentee))
        |> Seq.groupBy fst
        |> Seq.map (fun (skillPeriod, group) ->  skillPeriod, group |> Seq.map snd |> Set.ofSeq)
        |> Map.ofSeq


    let private getSkillValue (mentees:Mentee seq) (mentors:Mentor seq) =
        
        let menteeSkills =
            mentees
            |> Seq.collect (fun m -> m.Skills)

        let mentorSkills =
            mentors
            |> Seq.collect (fun m -> m.Skills)

        let skillCounts =
            Seq.append menteeSkills mentorSkills
            |> Seq.countBy id

        let numberOfSkills = Seq.length skillCounts

        skillCounts
        |> Seq.sortBy snd
        |> Seq.mapi (fun i (skill, _) -> skill, float (numberOfSkills - i))
        |> Map.ofSeq


    let private buildModel (mentees:Mentee seq) (mentors:Mentor seq) (skillValue:SMap<Skill,float>) (pairingDecision:SMap4<Skill,Period,Mentor,Mentee,Decision>) =


        let menteeSingleAssignmentConstraints =
            ConstraintBuilder "MenteeSingleAssignment" {
                for mentee in mentees ->
                    sum pairingDecision.[All, All, All, mentee] <== 1.0
            }

        let mentorMaxAssignmentConstraints =
            ConstraintBuilder "MentorMaxAssignments" {
                for mentor in mentors ->
                    let (MenteeCapacity menteeCapacity) = mentor.MaxMentees
                    sum pairingDecision.[All, All, mentor, All] <== float menteeCapacity
            }

        let mentorSingleAssignmentForPeriodConstraints =
            ConstraintBuilder "MentorSingleAssignmentForPeriod" {
                for mentor in mentors do
                    for period in mentor.Periods ->
                        let x = pairingDecision.[All, period, mentor, All]
                        sum pairingDecision.[All, period, mentor, All] <== 1.0
            }

        let assignmentValueExpr = sum (skillValue .* pairingDecision)
        let maxAssignmentValueObjecitve =
            Objective.create "MaxAssignmentValue" Maximize assignmentValueExpr

        let model =
            Model.create maxAssignmentValueObjecitve
            |> Model.addConstraints menteeSingleAssignmentConstraints
            |> Model.addConstraints mentorMaxAssignmentConstraints
            |> Model.addConstraints mentorSingleAssignmentForPeriodConstraints

        model


    let findOptimalPairing (mentees:Mentee seq) (mentors:Mentor seq) =
    
        let periods = getPeriods mentees mentors
        let skills = getSkills mentees mentors
        let skillPeriodToMentors = getSkillPeriodToMentorsMap mentors
        let skillPeriodToMentees = getSkillPeriodToMenteesMap mentees
        let skillValue = getSkillValue mentees mentors |> SMap

        let pairingDecision =
            DecisionBuilder "Pair" {
                for skill in skills do
                for period in periods do
                for mentor in Map.tryOrDefault Set.empty (period, skill) skillPeriodToMentors do
                for mentee in Map.tryOrDefault Set.empty (period, skill) skillPeriodToMentees ->
                    Boolean
            } |> SMap4

        let model = buildModel mentees mentors skillValue pairingDecision

        let solveSettings = {
            SolverType = CBC
            MaxDuration = 1_000L
            WriteLPFile = None
        }

        let result = Solver.solve solveSettings model

        match result with
        | Optimal sln ->
            Solution.getValues sln (pairingDecision.AsMap())
            |> Map.toSeq
            |> Seq.filter (fun (k, v) -> v >= 1.0) // Get the Pairings that the solver selected
            |> Seq.map fst // Return just the tuple representing the pairing
            |> Result.Ok
        | _ -> Result.Error "Error Solving"
            

