namespace MentorMatching

module Types =
    
    type MentorId = MentorId of string
    type MenteeId = MenteeId of string
    type Skill = Skill of string
    type Period = Period of int
    type MenteeCapacity = MenteeCapacity of int

    type Mentor = {
        MentorId : MentorId
        Skills : Skill Set
        Periods : Period Set
        MaxMentees : MenteeCapacity
    }

    type Mentee = {
        MenteeId : MenteeId
        Skills : Skill Set
        Periods : Period Set
    }
