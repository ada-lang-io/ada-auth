package Formatter.Clauses is
   function Is_Top_Level_Clause (Clause_Number : String) return Boolean;
   function Find_Top_Level_Clause (Clause_Number : String) return String;
   function Simplify_Clause_Number (Clause_Number : String) return String;
   function Directory_For_Clause (File_Prefix : String; Clause_Number : String) return String;
   function Make_Clause_File_Stem (File_Prefix : String; Clause_Number : String) return String;
   function Make_Clause_File_Name (File_Prefix : String; Clause_Number : String) return String;
   function Make_Clause_Anchor_Inner_Target (Clause_Number : String) return String;
   function Make_Clause_Anchor (File_Prefix : String; Clause_Number : String) return String;
end Formatter.Clauses;