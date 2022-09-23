with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;

package body Formatter.Clauses is
   function Is_Front_Matter_Clause (Clause_Number : String) return Boolean is
      (Clause_Number = "TOC" or else Clause_Number = "Title");

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

   function Directory_For_Clause (
      File_Prefix : String;
      Clause_Number : String)
   return String is
   begin
      return (if Clause_Number /= ""
         then File_Prefix & "-" & Find_Top_Level_Clause (Simplify_Clause_Number (Clause_Number)) & "/"
         else "");
   end Directory_For_Clause;

   function Make_Clause_File_Stem (
      File_Prefix : String;
      Clause_Number : String) return String
   is
      Dot_Set : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ('.');
      Dot_Index : constant Natural := Ada.Strings.Fixed.Index (Clause_Number, Dot_Set);
      Prepend : constant String := File_Prefix & "-";
   begin
      if Dot_Index /= 0 then
         declare
            Sub_Dot_Index : constant Natural := Ada.Strings.Fixed.Index (Clause_Number, Dot_Set, From => Dot_Index + 1);
         begin
            return (if Sub_Dot_Index /= 0
               then Prepend & Clause_Number (Clause_Number'First .. Sub_Dot_Index - 1)
               else Prepend & Clause_Number
            );
         end;
      else
         return Prepend & Clause_Number;
      end if;
   end Make_Clause_File_Stem;

   function Make_Clause_File_Name (File_Prefix : String; Clause_Number : String) return String
   is (Directory_For_Clause (File_Prefix, Clause_Number) & Make_Clause_File_Stem (File_Prefix, Clause_Number));

   function Make_Clause_Anchor_Inner_Target (Clause_Number : String) return String is
      Dot_Set : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ('.');
      Dot_Index : constant Natural := Ada.Strings.Fixed.Index (Clause_Number, Dot_Set);
   begin
      if Dot_Index /= 0 then
         declare
            Sub_Dot_Index : constant Natural := Ada.Strings.Fixed.Index (Clause_Number, Dot_Set, From => Dot_Index + 1);
         begin
            return (if Sub_Dot_Index /= 0
               then "Subclause_" & Clause_Number
               else ""
            );
         end;
      else
         return "";
      end if;
   end Make_Clause_Anchor_Inner_Target;

   function Make_Clause_Anchor (File_Prefix : String; Clause_Number : String) return String
   is
      Maybe_Target : constant String := Make_Clause_Anchor_Inner_Target (Clause_Number);
      Anchor_Name : constant String := (if Maybe_Target = "" then "" else "#" & Maybe_Target);
   begin
      return (if Maybe_Target = "" then Directory_For_Clause (File_Prefix, Clause_Number)
         else Make_Clause_File_Name (File_Prefix, Clause_Number) & Anchor_Name);
   end Make_Clause_Anchor;

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

   pragma Assert (Directory_For_Clause ("AA", "A") = "AA-A/");
   pragma Assert (Directory_For_Clause ("AA", "1.2.3") = "AA-1/");
   pragma Assert (Directory_For_Clause ("AA", "10.1") = "AA-10/");

   pragma Assert (Make_Clause_File_Stem ("AA", "A") = "AA-A");
   pragma Assert (Make_Clause_File_Stem ("AA", "10.1") = "AA-10.1");
   pragma Assert (Make_Clause_File_Stem ("AA", "1.2.3") = "AA-1.2");

   pragma Assert (Make_Clause_File_Name ("AA", "A") = "AA-A/AA-A");
   pragma Assert (Make_Clause_File_Name ("AA", "10.1") = "AA-10/AA-10.1");
   pragma Assert (Make_Clause_File_Name ("AA", "1.2.3") = "AA-1/AA-1.2");

   -- pragma Assert (Make_Clause_Anchor_Inner_Target ("A") = "");
   -- pragma Assert (Make_Clause_Anchor_Inner_Target ("A.1") = "");
   -- pragma Assert (Make_Clause_Anchor_Inner_Target ("A.1.2") = "Subclause_2");
   -- pragma Assert (Make_Clause_Anchor_Inner_Target ("TOC") = "");
   -- pragma Assert (Make_Clause_Anchor_Inner_Target ("") = "");
   -- pragma Assert (Make_Clause_Anchor_Inner_Target ("1.2") = "");
   -- pragma Assert (Make_Clause_Anchor_Inner_Target ("1.2.3.4") = "Subclause_3.4");

   null;

end Formatter.Clauses;