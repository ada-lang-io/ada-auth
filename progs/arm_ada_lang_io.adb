with ARM_Output;
with ARM_Contents;
with Ada.Directories;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;
use Ada.Text_IO;

with Formatter.Clauses;  use Formatter.Clauses;
with Formatter.JSX;

package body ARM_Ada_Lang_IO is
   --  Identifies code blocks requiring a <CodeBlock> tag.
   subtype Code_Block_Style is ARM_Output.Paragraph_Style_Type range ARM_Output.Examples .. ARM_Output.Small_Swiss_Examples;
   subtype Admonition_Style is ARM_Output.Paragraph_Style_Type range ARM_Output.Small .. ARM_Output.Small;

   function "+"(S : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;
   --  function "+"(S : String) renames Ada.Strings.Unbounded.To_Unbounded_String;

   ----------------------------------------------------------------------------

   package Paragraph_Buffer is
      procedure Append (Self : in out Ada_Lang_IO_Output_Type; Char : Character);
      procedure Append (Self : in out Ada_Lang_IO_Output_Type; S : String);
      procedure Backspace (Self : in out Ada_Lang_IO_Output_Type; Count : Positive);
   end Paragraph_Buffer;

   package body Paragraph_Buffer is
      procedure Append (Self : in out Ada_Lang_IO_Output_Type; Char : Character) is
      begin
         Ada.Strings.Unbounded.Append (Self.Buffer, Formatter.JSX.Safe_Char (Self.In_Block_Tag, Char));
      end Append;

      procedure Append (Self : in out Ada_Lang_IO_Output_Type; S : String) is
      begin
         Ada.Strings.Unbounded.Append (Self.Buffer, S);
      end Append;

      procedure Backspace (Self : in out Ada_Lang_IO_Output_Type; Count : Positive) is
         Last_Index : constant Positive := Ada.Strings.Unbounded.Length (Self.Buffer);
      begin
         Self.Buffer := Ada.Strings.Unbounded.Delete (Self.Buffer, Last_Index - Count + 1, Last_Index);
      end Backspace;
   end Paragraph_Buffer;

   ----------------------------------------------------------------------------

   package Immediate is
      procedure Put_Line (Self : in out Ada_Lang_IO_Output_Type; S : String);
      procedure Put (Self : in out Ada_Lang_IO_Output_Type; Char : Character);
      procedure Put (Self : in out Ada_Lang_IO_Output_Type; S : String);
      procedure New_Line (Self : in out Ada_Lang_IO_Output_Type; Count : Ada.Text_IO.Positive_Count := 1);
   end Immediate;

   package body Immediate is
      procedure Put (Self : in out Ada_Lang_IO_Output_Type; Char : Character) is
      begin
         Put (Self.Current_File, Char);
      end Put;

      procedure Put (Self : in out Ada_Lang_IO_Output_Type; S : String) is
      begin
         Put (Self.Current_File, S);
      end Put;

      procedure Put_Line (Self : in out Ada_Lang_IO_Output_Type; S : String) is
      begin
         Put_Line (Self.Current_File, S);
      end Put_Line;

      procedure New_Line (Self : in out Ada_Lang_IO_Output_Type; Count : Ada.Text_IO.Positive_Count := 1) is
      begin
         New_Line (Self.Current_File, Count);
      end New_Line;
   end Immediate;

   ----------------------------------------------------------------------------

   procedure Make_New_Sidebar (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      Immediate.Put_Line (Self, "---");
      Immediate.Put_Line (Self, "sidebar_position: " & Self.Next_Sidebar_Position'Image);
      Immediate.Put_Line (Self, "---");
      Self.Next_Sidebar_Position := Self.Next_Sidebar_Position + 1;
   end Make_New_Sidebar;

   procedure Print_Manual_Warning (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      Immediate.Put_Line (Self, ":::warning");
      Immediate.Put_Line (Self, "This Reference Manual output has not been verified,");
      Immediate.Put_Line (Self, "and may contain omissions or errors.");
      Immediate.Put_Line (Self, "Report any problems on the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)");
      Immediate.Put_Line (Self, ":::");
   end Print_Manual_Warning;

   procedure Include_React_Elements (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      Immediate.New_Line (Self);
      Immediate.Put_Line (Self, "import Admonition from ""@theme/Admonition"";");
      Immediate.Put_Line (Self, "import AnnotatedOnly from ""@site/src/components/AnnotatedOnly"";");
      Immediate.Put_Line (Self, "import CodeBlock from ""@theme/CodeBlock"";");
      Immediate.Put_Line (Self, "import MarginText from ""@site/src/components/MarginText"";");
      Immediate.Put_Line (Self, "import MarginInfo from ""@site/src/components/MarginInfo"";");
      Immediate.New_Line (Self);
   end Include_React_Elements;

   ----------------------------------------------------------------------------

   package Debugging is
      function Format_To_String (Format : ARM_Output.Format_Type) return String;
      function Paragraph_To_String (Paragraph : Paragraph_Styling) return String;
      procedure Trace (Self : in out Ada_Lang_IO_Output_Type; S : String);
   end Debugging;

   package body Debugging is
      function Format_To_String (Format : ARM_Output.Format_Type) return String is
      begin
         return (
            (if Format.Bold then "B" else " ")
            & (if Format.Italic then "I" else " ")
            & (case Format.Font is
               when ARM_Output.Roman => "R",
               when ARM_Output.Swiss => "S",
               when ARM_Output.Fixed => "F",
               when ARM_Output.Default => "D")
            & Format.Size'Image
         );
      end Format_To_String;

      function Paragraph_To_String (Paragraph : Paragraph_Styling) return String is
      begin
         return (
            Paragraph.Style'Image
            & " " & Paragraph.Indent'Image
            & " " & (+Paragraph.Number)
            & " " & Paragraph.Space_After'Image
            & " " & Paragraph.Justification'Image
         );
      end Paragraph_To_String;

      procedure Trace (Self : in out Ada_Lang_IO_Output_Type; S : String) is
      begin
         --  Put_Line (Self, "@@@ " & S);
         pragma Unreferenced (Self);
         pragma Unreferenced (S);
      end Trace;
   end Debugging;

   ----------------------------------------------------------------------------

   procedure End_Paragraph_Style (
      Self : in out Ada_Lang_IO_Output_Type;
      Style : ARM_Output.Paragraph_Style_Type) is
   begin
      case Style is
         when Code_Block_Style =>
            Immediate.New_Line (Self);
            Immediate.Put_Line (Self, "</CodeBlock>");
         when ARM_Output.Small
         | ARM_Output.Small_Wide_Above =>
            Immediate.Put_Line (Self, "</Admonition>");
         when ARM_Output.Text_Prefixed_Style_Subtype =>
            Immediate.Put_Line (Self, "</dl>");
         when ARM_Output.Bullet_Prefixed_Style_Subtype =>
            Immediate.Put_Line (Self, "</ul>");
         when others =>
            if Ada.Strings.Unbounded.Index (Self.Buffer, "::=") /= 0 then
               Immediate.New_Line (Self);
               Immediate.Put_Line (Self, "</CodeBlock>");
            else
               Immediate.Put_Line (Self, "</p>");
            end if;
      end case;

      Self.Last_Was_AI_Reference := False;
      Self.Mergable_Paragraph := False;
      Self.Being_Merged := False;
   end End_Paragraph_Style;

   procedure Put_Heading (Self : in out Ada_Lang_IO_Output_Type; S : String) is
   begin
      if Self.Mergable_Paragraph then
         End_Paragraph_Style (Self, Self.Current_Paragraph.Style);
      end if;

      Immediate.New_Line (Self);
      Immediate.Put_Line (Self, S);
      Immediate.New_Line (Self);
   end Put_Heading;

   -- Make an id to jump to if there's an appropriate subclause which can be
   -- jumped to
   procedure Make_Clause_Target (Self : in out Ada_Lang_IO_Output_Type; Clause_Number : String) is
      Anchor_Target : constant String := Make_Clause_Anchor_Inner_Target (Clause_Number);
   begin
      if Anchor_Target /= "" then
         Immediate.Put_Line (Self, Formatter.JSX.Anchor (Anchor_Target, ""));
      end if;
   end Make_Clause_Target;

   ----------------------------------------------------------------------------

   package Files is
      procedure Start_File (
         Self : in out Ada_Lang_IO_Output_Type;
         File_Name : String;
         Clause_Number : String;
         Header_Text : String);
      procedure Close_File (Self : in out Ada_Lang_IO_Output_Type);
   end Files;

   package body Files is   
      procedure Start_File (
         Self : in out Ada_Lang_IO_Output_Type;
         File_Name : String;
         Clause_Number : String;
         Header_Text : String)
      is
         Dir : constant String := +Self.Output_Path & Directory_For_Clause (+Self.File_Prefix, Clause_Number);
      begin
         Close_File (Self);

         if not Ada.Directories.Exists (Dir) then
            Debugging.Trace (Self, "Creating new directory: " & Dir);
            Ada.Directories.Create_Path (Dir);
         end if;

         -- Open new file
         Ada.Text_IO.Create (Self.Current_File, Ada.Text_IO.Out_File, Dir & File_Name);

         Make_New_Sidebar (Self);

         Put_Heading (Self, "# " & Clause_Number & " " & Header_Text);

         Print_Manual_Warning (Self);
         Include_React_Elements (Self);

         Self.Mergable_Paragraph := False;
         Self.Being_Merged := False;
      end Start_File;
      
      procedure Close_File (Self : in out Ada_Lang_IO_Output_Type) is
      begin
         -- Close previous file (if exists)
         if Ada.Text_IO.Is_Open (Self.Current_File) then
            if Self.Mergable_Paragraph then
               End_Paragraph_Style (Self, Self.Current_Paragraph.Style);
            end if;

            Ada.Text_IO.Close (Self.Current_File);
         end if;
      end Close_File;
   end Files;

   ----------------------------------------------------------------------------

   -- Create an Self for a document.
   -- The prefix of the output file names is File_Prefix - this
   -- should be no more then 5 characters allowed in file names.
   -- The result files will be written to Output_Path.
   -- The title of the document is Title.
   procedure Create(
     Self : in out Ada_Lang_IO_Output_Type;
     File_Prefix : in String;
     Output_Path : in String;
     Title : in String := "";
     Verbose : Boolean := True)
   is
      Dir : constant String := +Self.Output_Path & Directory_For_Clause (+Self.File_Prefix, "");
   begin
      Self.File_Prefix := Ada.Strings.Unbounded.To_Unbounded_String (File_Prefix);
      Self.Output_Path := Ada.Strings.Unbounded.To_Unbounded_String (Output_Path);

      Files.Start_File (Self, File_Prefix & "-Title.mdx", "", Title);

      Self.Verbose := Verbose;
   end Create;

   -- Close an Self. No further output to the object is
   -- allowed after this call.
   procedure Close (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      null;
   end Close;

   -- Start a new section. The title is Section_Title (this is
   -- intended for humans). The name is Section_Name (this is
   -- intended to be suitable to be a portion of a file name).
   procedure Section(
      Self : in out Ada_Lang_IO_Output_Type;
      Section_Title : in String;
      Section_Name  : in String
   ) is
   begin
      Debugging.Trace (Self, "Section");
      Debugging.Trace (Self, "Section Title: " & Section_Title);
      Debugging.Trace (Self, "Section Name:" & Section_Name);
      null;
   end Section;

   -- Set the number of columns.
   procedure Set_Columns (
      Self     : in out Ada_Lang_IO_Output_Type;
      Number_of_Columns : in ARM_Output.Column_Count
   ) is
   begin
      Debugging.Trace (Self, "Set_Columns");
      Debugging.Trace (Self, "Number of columns: " & Number_of_Columns'Image);
   end Set_Columns;

   function Is_Mergable_Paragraph (Style : ARM_Output.Paragraph_Style_Type) return Boolean is
   begin
      return Style in Code_Block_Style
         or else Style in ARM_Output.Text_Prefixed_Style_Subtype
         or else Style in ARM_Output.Bullet_Prefixed_Style_Subtype;
   end Is_Mergable_Paragraph;

   function Can_Merge_Paragraphs (Previous, Next : ARM_Output.Paragraph_Style_Type) return Boolean is
      use type ARM_Output.Paragraph_Style_Type;
   begin
      return Is_Mergable_Paragraph(Previous) and then Previous = Next;
   end Can_Merge_Paragraphs;

   -- Start a new paragraph. The style and indent of the paragraph is as
   -- specified. The (AA)RM paragraph number (which might include update
   -- and version numbers as well: [12.1/1]) is Number. If the format is
   -- a type with a prefix (bullets, hangining items), the prefix is
   -- omitted if No_Prefix is true. Tab_Stops defines the tab stops for
   -- the paragraph. If No_Breaks is True, we will try to avoid page breaks
   -- in the paragraph. If Keep_with_Next is true, we will try to avoid
   -- separating this paragraph and the next one. (These may have no
   -- effect in formats that don't have page breaks). Space_After
   -- specifies the amount of space following the paragraph. Justification
   -- specifies the text justification for the paragraph. Not_Valid_Error
   -- is raised if Tab_Stops /= NO_TABS for a hanging or bulleted format.
   procedure Start_Paragraph
     (Self           : in out Ada_Lang_IO_Output_Type;
      Style          : in ARM_Output.Paragraph_Style_Type;
      Indent         : in ARM_Output.Paragraph_Indent_Type;
      Number         : in String;
      No_Prefix      : in Boolean := False;
      Tab_Stops      : in ARM_Output.Tab_Info := ARM_Output.NO_TABS;
      No_Breaks      : in Boolean := False;
      Keep_with_Next : in Boolean := False;
      Space_After    : in ARM_Output.Space_After_Type := ARM_Output.Normal;
      Justification  : in ARM_Output.Justification_Type := ARM_Output.Default
   ) is
      New_Paragraph : constant Paragraph_Styling := (
         Style => Style,
         Indent => Indent,
         Number => Ada.Strings.Unbounded.To_Unbounded_String (Number),
         No_Prefix => No_Prefix,
         Tab_Stops => Tab_Stops,
         No_Breaks => No_Breaks,
         Keep_with_Next => Keep_with_Next,
         Space_After => Space_After,
         Justification => Justification
      );
   begin
      Debugging.Trace (Self, "Start_Paragraph: " & Debugging.Paragraph_To_String (New_Paragraph));

      if Number /= "0" then
         Immediate.Put_Line (Self, "<AnnotatedOnly>");
         Immediate.Put_Line (Self, "<MarginText>");
         Immediate.Put_Line (Self, Number);
         Immediate.Put_Line (Self, "</MarginText>");
         Immediate.Put_Line (Self, "</AnnotatedOnly>");
      end if;

      if Self.Mergable_Paragraph then
         if Can_Merge_Paragraphs (Self.Current_Paragraph.Style, New_Paragraph.Style) then
            Self.Being_Merged := True;
         else
            End_Paragraph_Style (Self, Self.Current_Paragraph.Style);
            Self.Being_Merged := False;
         end if;
      else
         Self.Being_Merged := False;
      end if;

      Self.Current_Paragraph := New_Paragraph;
      Self.Mergable_Paragraph := Is_Mergable_Paragraph (Self.Current_Paragraph.Style);
      Self.In_Block_Tag := Style in Code_Block_Style;
   end Start_Paragraph;

   procedure End_Paragraph (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      Paragraph_Buffer.Append (Self, Ada.Characters.Latin_1.LF);

      -- Outputs the current buffer in the current format.
      if not (for all X in 1 .. Ada.Strings.Unbounded.Length (Self.Buffer)
               => Ada.Strings.Unbounded.Element (Self.Buffer, X) = ' ')
      then
         -- Ignore glossary definitions
         if Ada.Strings.Unbounded.Index (Self.Buffer, "Version=") /= 1 then
            if not Self.Being_Merged then
               case Self.Current_Paragraph.Style is
                  when Code_Block_Style =>
                     Immediate.Put_Line (Self, "<CodeBlock language=""ada"">");
                  when ARM_Output.Small
                  | ARM_Output.Small_Wide_Above =>
                     Immediate.Put_Line (Self, "<Admonition "
                        & "type=""aarm"""
                        & " aarm=""" & Admonition_Output (Self.Admonition_Format).all & """"
                        & " title=""" & Admonition_Texts (Self.Admonition_Format).all & """"
                        & ">");
                  when ARM_Output.Text_Prefixed_Style_Subtype =>
                     Immediate.Put_Line (Self, "<dl>");
                  when ARM_Output.Bullet_Prefixed_Style_Subtype =>
                     Immediate.Put_Line (Self, "<ul>");
                  when others =>
                     if Ada.Strings.Unbounded.Index (Self.Buffer, "::=") /= 0 then
                        Immediate.New_Line (Self);
                        Immediate.Put_Line (Self, "<CodeBlock>");
                        Self.Current_Paragraph.Style := ARM_Output.Examples;
                        Self.Mergable_Paragraph := Is_Mergable_Paragraph (Self.Current_Paragraph.Style);
                        Self.In_Block_Tag := Self.Current_Paragraph.Style in Code_Block_Style;
                     else
                        Immediate.Put (Self, "<p>");
                     end if;
               end case;
            end if;

            case Self.Current_Paragraph.Style is
               when ARM_Output.Text_Prefixed_Style_Subtype =>
                  Immediate.Put (Self, "<dd>");
                  Immediate.Put (Self, +Self.Buffer);
                  Immediate.Put (Self, "</dd>");
               when ARM_Output.Bullet_Prefixed_Style_Subtype =>
                  Immediate.Put (Self, "<li>");
                  Immediate.Put (Self, +Self.Buffer);
                  Immediate.Put (Self, "</li>");
               when others =>
                  Immediate.Put (Self, +Self.Buffer);
            end case;

            if not Is_Mergable_Paragraph (Self.Current_Paragraph.Style) then
               End_Paragraph_Style (Self, Self.Current_Paragraph.Style);
            else
               self.Mergable_Paragraph := True;
            end if;

            Immediate.New_Line (Self);
         end if;
      end if;

      Self.Buffer := Ada.Strings.Unbounded.Null_Unbounded_String;
      Self.Admonition_Format := Note;

      Debugging.Trace (Self, "End_Paragraph");

      Self.In_Block_Tag := False;
   end End_Paragraph;

   -- Output a Category header (that is, "Legality Rules",
   -- "Dynamic Semantics", etc.)
   -- (Note: We did not use a enumeration here to insure that these
   -- headers are spelled the same in all output versions).
   -- Raises Not_Valid_Error if in a paragraph.
   procedure Category_Header (
      Self : in out Ada_Lang_IO_Output_Type;
      Header_Text : String
   ) is
   begin
      Put_Heading (Self, "#### " & Header_Text);
   end Category_Header;

   -- Output a Clause header. The level of the header is specified
   -- in Level. The Clause Number is as specified; the top-level (and
   -- other) subdivision names are as specified. These should appear in
   -- the table of contents.
   -- For hyperlinked formats, this should generate a link target.
   -- If No_Page_Break is True, suppress any page breaks.
   -- Raises Not_Valid_Error if in a paragraph.
   procedure Clause_Header
     (Self : in out Ada_Lang_IO_Output_Type;
      Header_Text : in String;
      Level : in ARM_Contents.Level_Type;
      Clause_Number : in String;
      Top_Level_Subdivision_Name : in ARM_Output.Top_Level_Subdivision_Name_Kind;
      No_Page_Break : in Boolean := False)
   is
      Annex_String : constant String := "Annex ";
      File_Name : String := "AA-"
         & (if Clause_Number = "" and then
            (Header_Text = "Table of Contents" or else -- Ada 95 format
		       Header_Text = "Contents") then "TOC"
            else
               (if Ada.Strings.Fixed.Index (Clause_Number, Annex_String) = 1
               then Clause_Number (Annex_String'Last + 2 - Clause_Number'First .. Clause_Number'Last)
               else Clause_Number)
            )
      & ".mdx";
   begin
      case Level is
         when ARM_Contents.Section
         | ARM_Contents.Plain_Annex
         | ARM_Contents.Informative_Annex
         | ARM_Contents.Normative_Annex =>
            Files.Start_File (Self, File_Name, Clause_Number, Header_Text);
         when ARM_Contents.Clause =>
            Files.Start_File (Self, "AA-" & Clause_Number & ".mdx", Clause_Number, Header_Text);
         when ARM_Contents.Subclause =>
            Make_Clause_Target (Self, Clause_Number);
            Put_Heading (Self, "## " & Clause_Number & "  " & Header_Text);
         when ARM_Contents.Subsubclause =>
            Make_Clause_Target (Self, Clause_Number);
            Put_Heading (Self, "### " & Clause_Number & "  " & Header_Text);
         when others =>
            null;
      end case;
   end Clause_Header;

   procedure Revised_Clause_Header
     (Self                       : in out Ada_Lang_IO_Output_Type;
      New_Header_Text            : in String;
      Old_Header_Text            : in String;
      Level                      : in ARM_Contents.Level_Type;
      Clause_Number              : in String;
      Version                    : in ARM_Contents.Change_Version_Type;
      Old_Version                : in ARM_Contents.Change_Version_Type;
      Top_Level_Subdivision_Name : in ARM_Output.Top_Level_Subdivision_Name_Kind;
      No_Page_Break              : in Boolean := False) is
   begin
      Debugging.Trace (Self, "Revised_Clause_Header");
      Debugging.Trace (Self, "Old header text: " & Old_Header_Text);
      Debugging.Trace (Self, "Clause Number: " & Clause_Number);
      Debugging.Trace (Self, "Old Version: " & Old_Version'Image);
      Debugging.Trace (Self, "Top Level Subdivision Name" & Top_Level_Subdivision_Name'Image);

      Clause_Header (Self, New_Header_Text, Level, Clause_Number, Top_Level_Subdivision_Name, No_Page_Break);
   end Revised_Clause_Header;

   -- Mark the start (if For_Start is True) or end (if For_Start is
   -- False) of the table of contents data. Output objects that
   -- auto-generate the table of contents can use this to do needed
   -- actions.
   procedure TOC_Marker (
      Self : in out Ada_Lang_IO_Output_Type;
      For_Start : in Boolean
   ) is
   begin
      pragma Unreferenced (Self);
      pragma Unreferenced (For_Start);
      null;
   end TOC_Marker;

   -- Output a column break.
   -- Raises Not_Valid_Error if in a paragraph, or if the number of
   -- columns is 1.
   procedure New_Column (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      pragma Unreferenced (Self);
   end New_Column;

   procedure Start_Table
     (Self      : in out Ada_Lang_IO_Output_Type;
      Columns            : in ARM_Output.Column_Count;
      First_Column_Width : in ARM_Output.Column_Count;
      Last_Column_Width  : in ARM_Output.Column_Count;
      Alignment          : in ARM_Output.Column_Text_Alignment;
      No_Page_Break      : in Boolean; Has_Border : in Boolean;
      Small_Text_Size    : in Boolean;
      Header_Kind        : in ARM_Output.Header_Kind_Type)
   is
   begin
      Debugging.Trace (Self, "Start_Table");
      Debugging.Trace (Self, "Columns: " & Columns'Image);
      Debugging.Trace (Self, "First Column Width: " & First_Column_Width'Image);
      Debugging.Trace (Self, "Last Column Width:  " & Last_Column_Width'Image);
      Debugging.Trace (Self, "Alignment:          " & Alignment'Image);
      Debugging.Trace (Self, "Header kind:        " & Header_Kind'Image);
   end Start_Table;

   procedure Table_Marker (
      Self : in out Ada_Lang_IO_Output_Type;
      Marker : in ARM_Output.Table_Marker_Type
   ) is
   begin
      Debugging.Trace (Self, "Table_Marker");
      Debugging.Trace (Self, "Marker: " & Marker'Image);
   end Table_Marker;

   -- Output a separator line. It is thin if "Is_Thin" is true.
   -- Raises Not_Valid_Error if in a paragraph.
   procedure Separator_Line
     (Self : in out Ada_Lang_IO_Output_Type; Is_Thin : Boolean := True)
   is
   begin
      Debugging.Trace (Self, "Separator_Line");
      Self.Last_Was_AI_Reference := False;
   end Separator_Line;

   -- Text output: These are only allowed after a Start_Paragraph and
   -- before any End_Paragraph. Raises Not_Valid_Error if not allowed.
   -- Output ordinary text.
   -- The text must end at a word break, never in the middle of a word.
   procedure Ordinary_Text
     (Self : in out Ada_Lang_IO_Output_Type; Text : in String)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      --  If this isn't an admonition, then output it.
      Self.Last_Was_AI_Reference := False;

      for Admonition in Admonition_Type loop
         if Text = Admonition_Texts (Admonition).all then
            Self.Admonition_Format := Admonition;
            return;
         end if;
      end loop;

      for Char of Text loop
         Paragraph_Buffer.Append (Self, Char);
      end loop;
   end Ordinary_Text;

   procedure Ordinary_Character
     (Self : in out Ada_Lang_IO_Output_Type; Char : in Character)
   is
   begin
      if not (Char = '}' or else Char = ' ' or else Char = '{' or else Char = Ada.Characters.Latin_1.LF)
         and then Self.Last_Was_AI_Reference and then Self.Current_Paragraph.Style not in Code_Block_Style
      then
         Line_Break (Self);
         Self.Last_Was_AI_Reference := False;
      end if;

      Paragraph_Buffer.Append (Self, Formatter.JSX.Safe_Char (Self.In_Block_Tag, Char));

      if Char = '}'
         and then Self.Last_Was_AI_Reference
         and then Self.Current_Paragraph.Style in Code_Block_Style
      then
         Line_Break (Self);
      end if;

   end Ordinary_Character;

   procedure Hard_Space (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      -- Just treat as a space.
      --  Debugging.Trace (Self, "Hard_Space");
      Ordinary_Character (Self, ' ');
      Self.Last_Was_AI_Reference := False;
   end Hard_Space;

   -- Output a line break. This does not start a new paragraph.
   -- This corresponds to a "<BR>" in HTML.
   procedure Line_Break (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      --  Debugging.Trace (Self, "Line_Break");
      --  Immediate.New_Line (Self, 1);
      Ordinary_Character (Self, Ada.Characters.Latin_1.LF);
      Self.Last_Was_AI_Reference := False;
   end Line_Break;

   -- Output a line break for the index. This does not start a new
   -- paragraph in terms of spacing. This corresponds to a "<BR>"
   -- in HTML. If Clear_Keep_with_Next is true, insure that the next
   -- line does not require the following line to stay with it.
   -- Raises Not_Valid_Error if the paragraph is not in the index format.
   procedure Index_Line_Break
     (Self : in out Ada_Lang_IO_Output_Type; Clear_Keep_with_Next : in Boolean)
   is
   begin
      pragma Unreferenced (Self);
      pragma Unreferenced (Clear_Keep_with_Next);
      --  Debugging.Trace (Self, "Index_Line_Break");
   end Index_Line_Break;

   -- Output a soft line break. This is a place (in the middle of a
   -- "word") that we allow a line break. It is usually used after
   -- underscores in long non-terminals.
   procedure Soft_Line_Break (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      -- Ignored since this is a web based format.
      pragma Unreferenced (Self);
      --  Debugging.Trace (Self, "Soft_Line_Break");
   end Soft_Line_Break;

   -- Output a soft line break, with a hyphen. This is a place (in the middle of
   -- a "word") that we allow a line break. If the line break is used,
   -- a hyphen will be added to the text.
   procedure Soft_Hyphen_Break (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      -- Soft hyphens are ignored.
      pragma Unreferenced (Self);
      --  Debugging.Trace (Self, "Soft_Hyphen_Break");
   end Soft_Hyphen_Break;

   -- Output a tab, inserting space up to the next tab stop.
   -- Raises Not_Valid_Error if the paragraph was created with
   -- Tab_Stops = ARM_Output.NO_TABS.
   procedure Tab (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      Ordinary_Character (Self, Ada.Characters.Latin_1.HT);
      --  Debugging.Trace (Self, "Tab");
   end Tab;

   -- Output an special character.
   procedure Special_Character
     (Self : in out Ada_Lang_IO_Output_Type;
      Char : in ARM_Output.Special_Character_Type)
   is
   begin
      --  Debugging.Trace (Self, "Special_Character");
      --  Debugging.Trace (Self, "Char: " & Char'Image);
      case Char is
         when ARM_Output.EM_Dash => Ordinary_Character (Self, '-');
         when ARM_Output.Left_Double_Quote => Ordinary_Character (Self, '"');
         when ARM_Output.Right_Double_Quote => Ordinary_Character (Self, '"');
         when ARM_Output.Right_Quote => Ordinary_Character (Self, ''');
         when ARM_Output.Left_Quote => Ordinary_Character (Self, ''');
         when others => null;

            --  EN_Dash, -- EN (long) dash
            --  GEQ, -- Greater than or equal symbol.
            --  LEQ, -- Less than or equal symbol.
            --  NEQ, -- Not equal symbol.
            --  PI,  -- PI.
            --  Left_Ceiling, -- Left half of ceiling.
            --  Right_Ceiling, -- Right half of ceiling.
            --  Left_Floor, -- Left half of floor.
            --  Right_Floor, -- Right half of floor.
            --  Thin_Space, -- A thinner than usual space.
            --  Left_Quote, -- A left facing single quote.
            --  Right_Quote, -- A right facing single quote.
            --  Left_Double_Quote, -- A left facing double quote.
            --  Right_Double_Quote, -- A right facing double quote.
            --  Small_Dotless_I, -- A small i without a dot (Unicode(16#0131#).
            --  Capital_Dotted_I -- A large I with a dot (Unicode(16#0130#).
      end case;
   end Special_Character;

      -- Output a Unicode character, with code position Char.
   procedure Unicode_Character
     (Self : in out Ada_Lang_IO_Output_Type; Char : in ARM_Output.Unicode_Type)
   is
      Char_Code : constant String := ARM_Output.Unicode_Type'Image (Char);
   begin
      Debugging.Trace (Self, "Unicode_Character");
      Debugging.Trace (Self, "Char_Code: " & Char_Code);
      Paragraph_Buffer.Append (Self, "&#" & Char_Code(2..Char_Code'Length) & ';');
   end Unicode_Character;

   -- Marks the end of a hanging item. Call only once per paragraph.
   -- Raises Not_Valid_Error if the paragraph style is not in
   -- Text_Prefixed_Style_Subtype, or if this has already been
   -- called for the current paragraph, or if the paragraph was started
   -- with No_Prefix = True.
   procedure End_Hang_Item (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      Debugging.Trace (Self, "End_Hang_Item");
      Immediate.Put_Line (Self, "<dt><br/>" & (+Self.Buffer) & "</dt>");
      Self.Buffer := Ada.Strings.Unbounded.Null_Unbounded_String;
   end End_Hang_Item;

   -- Change the text format so that all of the properties are as specified.
   -- Note: Changes to these properties ought be stack-like; that is,
   -- Bold on, Italic on, Italic off, Bold off is OK; Bold on, Italic on,
   -- Bold off, Italic off should be avoided (as separate commands).
   procedure Text_Format
     (Self : in out Ada_Lang_IO_Output_Type;
      Format : in ARM_Output.Format_Type)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
      use type ARM_Output.Font_Family_Type;
      use type ARM_Output.Format_Type;
   begin
      if Self.Last_Was_AI_Reference and then Format.Font = ARM_Output.Swiss then
         Line_Break (Self);
         Self.Last_Was_AI_Reference := False;
      end if;

      -- Not all admonitions appear at the start of a block, so check for
      -- formatting to know if we're in one or not.
      for Admonition in Admonition_Type loop
         declare
            Admonition_Index : constant Natural := Ada.Strings.Unbounded.Index (Self.Buffer, Admonition_Texts (Admonition).all);
         begin
            if Admonition_Index /= 0 then
               Self.Admonition_Format := Admonition;

               -- Assume admonition was immediately before the formatting.
               Self.Buffer := Ada.Strings.Unbounded.Unbounded_Slice (Self.Buffer, 1, Admonition_Index - 1);
            end if;
         end;
      end loop;

      if not Self.In_Block_Tag then
         -- Turn off any changed formatting before turning on any formatting
         -- and also turn off formatting in the inverse order as added
         -- formatting to keep stack-like behavior when multiple states change
         -- at once.
         if Format /= Self.Current_Format then
            if Format.Font /= ARM_Output.Swiss and then Self.Current_Format.Font = ARM_Output.Swiss then
               Paragraph_Buffer.Append (Self, "</code>");
            end if;

            if Format.Italic /= Self.Current_Format.Italic and then not Format.Italic then
               Paragraph_Buffer.Append (Self, "</em>");
            end if;

            if Format.Bold /= Self.Current_Format.Bold and then not Format.Bold then
               Paragraph_Buffer.Append (Self, "</strong>");
            end if;

            if Format.Bold /= Self.Current_Format.Bold and then Format.Bold then
               Paragraph_Buffer.Append (Self, "<strong>");
            end if;

            if Format.Italic /= Self.Current_Format.Italic and then Format.Italic then
               Paragraph_Buffer.Append (Self, "<em>");
            end if;

            if Format.Font = ARM_Output.Swiss and then Self.Current_Format.Font /= ARM_Output.Swiss then
               Paragraph_Buffer.Append (Self, "<code>");
            end if;
         end if;
      end if;

      --  Debugging.Trace (Self, "Text_Format");
      --  Debugging.Trace (Self, "Format: ");
      --  Debugging.Trace (Self, "Bold: " & Format.Bold'Image, 2);
      --  Debugging.Trace (Self, "Italic: " & Format.Italic'Image, 2);
      --  Debugging.Trace (Self, "Font: " & Format.Font'Image, 2);
      --  Debugging.Trace (Self, "Size: " & Format.Size'Image, 2);
      --  Debugging.Trace (Self, "Color: " & Format.Color'Image, 2);
      --  Debugging.Trace (Self, "Change: " & Format.Change'Image, 2);
      --  Debugging.Trace (Self, "Version: " & Format.Version'Image, 2);
      --  Debugging.Trace (Self, "Added_Version: " & Format.Added_Version'Image, 2);
      --  Debugging.Trace (Self, "Location: " & Format.Location'Image, 2);

      --  Debugging.Trace (Self, Format_To_String (Format));

      --  Should flush the last bit since the last text format change...
      --  but only when NOT writing code samples, since text formats
      --  are used in the code samples...
      --
      --  Immediate.Flush (Self);

      Self.Current_Format := Format;
   end Text_Format;

   -- Generate a reference to a clause in the standard. The text of
   -- the reference is "Text", and the number of the clause is
   -- Clause_Number. For hyperlinked formats, this should generate
   -- a link; for other formats, the text alone is generated.
   procedure Clause_Reference(
      Self : in out Ada_Lang_IO_Output_Type;
      Text : in String;
      Clause_Number : in String
   ) is
   begin
      --  Debugging.Trace (Self, "Clause_Reference");
      --  Debugging.Trace (Self, "Clause Number: " & Clause_Number);

      -- Ignore this by consuming the buffer.
      --  Self.Buffer := Ada.Strings.Unbounded.Null_Unbounded_String;

      Paragraph_Buffer.Append (Self, Formatter.JSX.Make_Link (Text, Make_Clause_Anchor (+Self.File_Prefix, Formatter.Clauses.Simplify_Clause_Number (Clause_Number)), Self.In_Block_Tag));
   end Clause_Reference;

   -- Generate a index target. This marks the location where an index
   -- reference occurs. Index_Key names the index item involved.
   -- For hyperlinked formats, this should generate a link target;
   -- for other formats, nothing is generated.
   procedure Index_Target
     (Self : in out Ada_Lang_IO_Output_Type; Index_Key : in Natural)
   is
   begin
      pragma Unreferenced (Self);
      pragma Unreferenced (Index_Key);
      --  Debugging.Trace (Self, "Index_Target");
      --  Debugging.Trace (Self, "Index Key: " & Index_Key'Image);
   end Index_Target;

   -- Generate a reference to an index target in the standard. The text
   -- of the reference is "Text", and Index_Key and Clause_Number denotes
   -- the target. For hyperlinked formats, this should generate
   -- a link; for other formats, the text alone is generated.
   procedure Index_Reference (
      Self          : in out Ada_Lang_IO_Output_Type;
      Text          : in String;
      Index_Key     : in Natural;
      Clause_Number : in String)
   is
   begin
      pragma Unreferenced (Index_Key);
      pragma Unreferenced (Clause_Number);

      --  Debugging.Trace (Self, "Index_Reference");
      --  Debugging.Trace (Self, "Text: " & Text);
      --  Debugging.Trace (Self, "Index_Key: " & Index_Key'Image);

      Paragraph_Buffer.Append (Self, Text);
   end Index_Reference;

   -- Generate a reference to an DR from the standard. The text
   -- of the reference is "Text", and DR_Number denotes
   -- the target. For hyperlinked formats, this should generate
   -- a link; for other formats, the text alone is generated.
   --
   -- These are numbers like 8652/0033
   procedure DR_Reference
     (Self : in out Ada_Lang_IO_Output_Type;
      Text : in String;
      DR_Number : in String)
   is
   begin
      --  Debugging.Trace (Self, "DR_Reference");
      --  Debugging.Trace (Self, "DR Number: " & DR_Number);

      Paragraph_Buffer.Append (Self, Text);
   end DR_Reference;

   -- Generate a reference to an AI from the standard. The text
   -- of the reference is "Text", and AI_Number denotes
   -- the target (in unfolded format). For hyperlinked formats, this should
   -- generate a link; for other formats, the text alone is generated.
   procedure AI_Reference
     (Self : in out Ada_Lang_IO_Output_Type;
      Text : in String;
      AI_Number : in String)
   is begin
      --  Debugging.Trace (Self, "AI_Reference");
      --  Debugging.Trace (Self, "Text: " & Text);
      --  Debugging.Trace (Self, "AI_Number: " & AI_Number);
      --  Paragraph_Buffer.Append (Self, (if Self.In_Block_Tag then Wrap (Text) else Text));

      if Self.Current_Paragraph.Style in Code_Block_Style then
         Paragraph_Buffer.Backspace (Self, 5);  -- Delete the previously emitted {"{"}
         Paragraph_Buffer.Append (Self, "--  ");
         Ordinary_Character (Self, '{');
      end if;

      Paragraph_Buffer.Append (Self, Formatter.JSX.Wrap (Text));

      Self.Last_Was_AI_Reference := True;
   end AI_Reference;

   -- Generate a local target. This marks the potential target of local
   -- links identified by "Target". Text is the text of the target.
   -- For hyperlinked formats, this should generate a link target;
   -- for other formats, only the text is generated.
   procedure Local_Target
     (Self : in out Ada_Lang_IO_Output_Type;
      Text : in String;
      Target : in String)
   is
   begin
      --  Debugging.Trace (Self, "Local_Target");
      --  Debugging.Trace (Self, "Text: " & Text);
      --  Debugging.Trace (Self, "Target: " & Target);

      Paragraph_Buffer.Append (Self, Formatter.JSX.Anchor (Target, Text));
   end Local_Target;

   -- Generate a local link to the target and clause given.
   -- Text is the text of the link.
   -- For hyperlinked formats, this should generate a link;
   -- for other formats, only the text is generated.
   procedure Local_Link (
      Self : in out Ada_Lang_IO_Output_Type;
      Text : in String;
      Target : in String;
      Clause_Number : in String)
   is
   begin
      --  Debugging.Trace (Self, "Local_Link");
      --  Debugging.Trace (Self, "Text: " & Text);
      --  Debugging.Trace (Self, "Target: " & Target);
      --  Debugging.Trace (Self, "Clause Number: " & Clause_Number);

      Paragraph_Buffer.Append (Self, Formatter.JSX.Make_Link (Text, "../" & Make_Clause_File_Name (+Self.File_Prefix, Clause_Number) & "#" & Target, Self.In_Block_Tag));
   end Local_Link;

   -- Generate a local link to the target and clause given.
   -- The link will surround text until Local_Link_End is called.
   -- Local_Link_End must be called before this routine can be used again.
   -- For hyperlinked formats, this should generate a link;
   -- for other formats, only the text is generated.
   procedure Local_Link_Start
     (Self : in out Ada_Lang_IO_Output_Type;
      Target : in String;
      Clause_Number : in String)
   is
   begin
      --  Debugging.Trace (Self, "Local_Link_Start");
      --  Debugging.Trace (Self, "Target: " & Target);
      --  Debugging.Trace (Self, "Clause Number: " & Clause_Number);

      -- todo: start link
      Paragraph_Buffer.Append (Self, "<a href=""" & "../" & Make_Clause_File_Name (+Self.File_Prefix, Clause_Number) & "#" & Target & """>");
   end Local_Link_Start;

   -- End a local link for the target and clause given.
   -- This must be in the same paragraph as the Local_Link_Start.
   -- For hyperlinked formats, this should generate a link;
   -- for other formats, only the text is generated.
   procedure Local_Link_End
     (Self : in out Ada_Lang_IO_Output_Type;
      Target : in String;
      Clause_Number : in String)
   is
   begin
      --  Debugging.Trace (Self, "Local_Link_End");
      --  Debugging.Trace (Self, "Target: " & Target);
      --  Debugging.Trace (Self, "Clause Number: " & Clause_Number);
      Paragraph_Buffer.Append (Self, "</a>");
   end Local_Link_End;

   -- Generate a link to the URL given.
   -- Text is the text of the link.
   -- For hyperlinked formats, this should generate a link;
   -- for other formats, only the text is generated.
   procedure URL_Link
     (Self : in out Ada_Lang_IO_Output_Type;
      Text : in String;
      URL : in String)
   is
   begin
      Debugging.Trace (Self, "URL_Link");
      Debugging.Trace (Self, "Text: " & Text);
      Debugging.Trace (Self, "URL: " & URL);

      Paragraph_Buffer.Append (Self, Formatter.JSX.Make_Link (Text, URL, Self.In_Block_Tag));
   end URL_Link;

   -- Generate a picture.
   -- Name is the (simple) file name of the picture; Descr is a
   -- descriptive name for the picture (it will appear in some web
   -- browsers).
   -- We assume that it is a .GIF or .JPG and that it will be present
   -- in the same directory as the input files and the same directory as
   -- the .HTML output files.
   -- Alignment specifies the picture alignment.
   -- Height and Width specify the picture size in pixels.
   -- Border specifies the kind of border.
   procedure Picture (
      Self : in out Ada_Lang_IO_Output_Type;
      Name          : in String;
      Descr         : in String;
      Alignment     : in ARM_Output.Picture_Alignment;
      Height, Width : in Natural;
      Border        : in ARM_Output.Border_Kind)
   is
   begin
      Debugging.Trace (Self, "Picture");
      Debugging.Trace (Self, "Name: " & Name);
      Debugging.Trace (Self, "Description: " & Descr);
      Debugging.Trace (Self, "Alignment: " & Alignment'Image);
      Debugging.Trace (Self, "Height: " & Height'Image);
      Debugging.Trace (Self, "Width: " & Width'Image);
      Debugging.Trace (Self, "Border: " & Border'Image);
   end Picture;

end ARM_Ada_Lang_IO;
