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

module Model =

    //let private getSkills (mentees:Mentee seq) (mentors:Mentor seq) =
    //    let menteeSkills =
    //        mentees
    //        |> Seq.collect (fun x -> x.Skills)
    //        |> Set.ofSeq

    //    let mentorSkills =
    //        mentors
    //        |> Seq.collect (fun x -> x.Skills)
    //        |> Set.ofSeq

    //    Set.intersect menteeSkills mentorSkills
    

    let private getSkillPeriods (mentees:Mentee seq) (mentors:Mentor seq) =
        let menteeSkills =
            mentees
            |> Seq.collect (fun m -> Set.combinations m.Periods m.Skills)
            |> Set.ofSeq

        let mentorSkills =
            mentors
            |> Seq.collect (fun m -> Set.combinations m.Periods m.Skills)
            |> Set.ofSeq

        Set.intersect menteeSkills mentorSkills

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


            


    let build (mentees:Mentee seq) (mentors:Mentor seq) =

        // This kids the intersect of Skill/Period pairs between Mentors and Mentees
        let periodSkillPairs = getSkillPeriods mentees mentors
        let skillPeriodToMentors = getSkillPeriodToMentorsMap mentors
        let skillPeriodToMentees = getSkillPeriodToMenteesMap mentees
        let skillValue = getSkillValue mentees mentors

        let periodSkillValue =
            periodSkillPairs
            |> Seq.map (fun (period, skill) -> (period, skill), skillValue.[skill])
            |> SMap

        let pairingDecision =
            DecisionBuilder "Pair" {
                for periodSkill in periodSkillPairs do
                    for mentor in skillPeriodToMentors.[periodSkill] do
                        for mentee in skillPeriodToMentees.[periodSkill] ->
                            Boolean
            } |> SMap3

        let menteeSingleAssignmentConstraints =
            ConstraintBuilder "MenteeSingleAssignment" {
                for mentee in mentees ->
                    sum pairingDecision.[All, All, mentee] <== 1.0
            }

        let mentorMaxAssignmentConstraints =
            ConstraintBuilder "MentorMaxAssignments" {
                for mentor in mentors ->
                    let (MenteeCapacity menteeCapacity) = mentor.MaxMentees
                    sum pairingDecision.[All, mentor, All] <== float menteeCapacity
            }

        let mentorSingleAssignmentForPeriodConstraints =
            ConstraintBuilder "MentorSingleAssignmentForPeriod" {
                for mentor in mentors do
                    for period in mentor.Periods ->
                        sum pairingDecision.[All, mentor]
            }


        ()
            

