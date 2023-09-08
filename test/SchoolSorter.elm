module SchoolSorter exposing (..)


sorterSchool : Sorter School
sorterSchool = (Sort.by .schoolId (let
                                       unId (Id value) =
                                           value
                                   in
                                   Sort.by unId Sort.increasing))
