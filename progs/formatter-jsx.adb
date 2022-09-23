with Ada.Characters.Latin_1;
with Ada.Strings.UTF_Encoding.Strings;

package body Formatter.JSX is
   function Wrap (S : String) return String is ( "{""" & S & """}");

   function Safe_Char (In_Block_Tag : Boolean; Char : Character) return String is
   begin
      case Char is
         when '<' => return Wrap ("<");
         when '>' => return Wrap (">");
         when '{' => return Wrap ("{");
         when '}' => return Wrap ("}");
         when Ada.Characters.Latin_1.LF => return (if In_Block_Tag then Wrap ("\n") else "<br />");
         when others => return Ada.Strings.UTF_Encoding.Strings.Encode ((1 => Char));
      end case;
   end Safe_Char;

   function Anchor (Target, Text : String) return String is
   begin
      return "<a id=""" & Target & """>" & Text & "</a>";
   end Anchor;

   function Make_Link (Name : String; Target : String; In_Block_Tag : Boolean) return String is
   begin
      return "<a href=""" & Target & """" & ">" & Name & "</a>";
   end Make_Link;
end Formatter.JSX;
