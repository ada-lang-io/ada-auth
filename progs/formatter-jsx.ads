package Formatter.JSX is
   function Wrap (S : String) return String;
   function Safe_Char (In_Block_Tag : Boolean; Char : Character) return String;
   function Anchor (Target, Text : String) return String;
   function Make_Link (Name : String; Target : String; In_Block_Tag : Boolean) return String;
end Formatter.JSX;