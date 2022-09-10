with Ada.Strings.Fixed;
with Ada.Strings.Maps;

package body Formatter.Clauses is

   function Is_Top_Level_Clause (Clause_Number : String) return Boolean is
      Dot_Set : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ('.');
   begin
      return Ada.Strings.Fixed.Index (Clause_Number, Dot_Set) = 0;
   end Is_Top_Level_Clause;

   function Find_Top_Level_Clause (Clause_Number : String) return String is
      Dot_Set : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ('.');
      Dot_Index : constant Natural := Ada.Strings.Fixed.Index (Clause_Number, Dot_Set);
   begin
      return (if Dot_Index = 0 then Clause_Number
         else Clause_Number (Clause_Number'First .. Dot_Index - 1));
   end Find_Top_Level_Clause;

   function Simplify_Clause_Number (Clause_Number : String) return String is
      Annex_String : constant String := "Annex ";
   begin
      return (if Ada.Strings.Fixed.Index (Clause_Number, Annex_String) = 1
         then Clause_Number (Annex_String'Last + 2 - Clause_Number'First .. Clause_Number'Last)
         else Clause_Number);
   end Simplify_Clause_Number;

begin

   pragma Assert (Is_Top_Level_Clause ("1"));
   pragma Assert (Is_Top_Level_Clause ("A"));
   pragma Assert (Is_Top_Level_Clause ("TOC"));
   pragma Assert (not Is_Top_Level_Clause ("1.2"));
   pragma Assert (not Is_Top_Level_Clause ("1.2.3"));
   pragma Assert (not Is_Top_Level_Clause ("A.1"));
   pragma Assert (not Is_Top_Level_Clause ("A.1.2"));

   pragma Assert (Find_Top_Level_Clause ("A") = "A");
   pragma Assert (Find_Top_Level_Clause ("A.1") = "A");
   pragma Assert (Find_Top_Level_Clause ("1.2") = "1");
   pragma Assert (Find_Top_Level_Clause ("1.2.3.4") = "1");

   pragma Assert (Simplify_Clause_Number ("Annex A") = "A");
   pragma Assert (Simplify_Clause_Number ("TOC") = "TOC");
   pragma Assert (Simplify_Clause_Number ("1.2") = "1.2");

   null;

end Formatter.Clauses;