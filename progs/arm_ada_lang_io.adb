with ARM_Output;
with ARM_Contents;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;
use Ada.Text_IO;

package body ARM_Ada_Lang_IO is
   --  Identifies code blocks requiring a <CodeBlock> tag.
   subtype Code_Block_Style is ARM_Output.Paragraph_Style_Type range ARM_Output.Examples .. ARM_Output.Small_Swiss_Examples;

   function JSX_Wrap (S : String) return String is ( "{""" & S & """}");

   function Safe_Char (In_Code_Block : Boolean; Char : Character) return String is
   begin
      case Char is
         when '<' => return JSX_Wrap ("<");  -- "&lt;";
         when '>' => return JSX_Wrap (">");  -- "&gt;";
         --  when '{' => return (if In_Code_Block then JSX_Wrap ("{") else "{");
         --  when '}' => return (if In_Code_Block then JSX_Wrap ("}") else "}");
         --  when Ada.Characters.Latin_1.LF => return (if In_Code_Block then JSX_Wrap ("\n") else (1 => Ada.Characters.Latin_1.LF));
         when '{' => return JSX_Wrap ("{");
         when '}' => return JSX_Wrap ("}");
         when Ada.Characters.Latin_1.LF => return (if In_Code_Block then JSX_Wrap ("\n") else ""); -- (1 => Ada.Characters.Latin_1.LF));
         when others => return (1 => Char);
      end case;
   end Safe_Char;

   package Detail is
      procedure Append (Self : in out Ada_Lang_IO_Output_Type; Char : Character);
      procedure Append (Self : in out Ada_Lang_IO_Output_Type; S : String);
      procedure Start_File (Self : in out Ada_Lang_IO_Output_Type; File_Name : String);
      procedure Put_Line (Self : in out Ada_Lang_IO_Output_Type; S : String);
      procedure New_Line (Self : in out Ada_Lang_IO_Output_Type; Count : Ada.Text_IO.Positive_Count := 1);
      procedure Flush (Self : in out Ada_Lang_IO_Output_Type);
      procedure Trace (Self : in out Ada_Lang_IO_Output_Type; S : String);
   end Detail;

   procedure Make_New_Sidebar (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      Detail.Put_Line (Self, "---");
      Detail.Put_Line (Self, "sidebar_position: " & Self.Next_Sidebar_Position'Image);
      Detail.Put_Line (Self, "---");
      Self.Next_Sidebar_Position := Self.Next_Sidebar_Position + 1;
   end Make_New_Sidebar;

   procedure Print_Manual_Warning (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      Detail.Put_Line (Self, ":::warning");
      Detail.Put_Line (Self, "We're still working on the Reference manual output.  Internal links are broken,");
      Detail.Put_Line (Self, "as are a bunch of other things.");
      Detail.Put_Line (Self, "See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)");
      Detail.Put_Line (Self, ":::");
   end Print_Manual_Warning;

   procedure Include_React_Elements (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      Detail.New_Line (Self);
      Detail.Put_Line (Self, "import CodeBlock from ""@theme/CodeBlock"";");
      Detail.New_Line (Self);
   end Include_React_Elements;

   function Make_Clause_File_Name (Clause_Number : String) return String is
      Dot_Set : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ('.');
      Dot_Index : constant Natural := Ada.Strings.Fixed.Index (Clause_Number, Dot_Set);
   begin
      if Dot_Index /= 0 then
         declare
            Sub_Dot_Index : constant Natural := Ada.Strings.Fixed.Index (Clause_Number, Dot_Set, From => Dot_Index + 1);
         begin
            return (if Sub_Dot_Index /= 0
               then "./AA-" & Clause_Number (Clause_Number'First .. Sub_Dot_Index - 1)
               else "./AA-" & Clause_Number
            );
         end;
      else
         return "./AA-" & Clause_Number;
      end if;
   end Make_Clause_File_Name;

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
         & " " & Ada.Strings.Unbounded.To_String (Paragraph.Number)
         & " " & Paragraph.Space_After'Image
         & " " & Paragraph.Justification'Image
      );
   end Paragraph_To_String;

   function Make_Link (Name : String; Target : String; In_Code_Block : Boolean) return String is
   begin
      return "<a href=""" & Target & """" & ">" & Name & "</a>";
      --  return (if In_Code_Block then "<a href=""" & Target & """" & ">" & Name & "</a>"
      --     else "[" & Name & "](" & Target & ")");
   end Make_Link;

   package body Detail is
      procedure Append (Self : in out Ada_Lang_IO_Output_Type; Char : Character) is
      begin
         Ada.Strings.Unbounded.Append (Self.Buffer, Safe_Char (Self.In_Code_Block, Char));
      end Append;

      procedure Append (Self : in out Ada_Lang_IO_Output_Type; S : String) is
      begin
         Ada.Strings.Unbounded.Append (Self.Buffer, S);
      end Append;

      procedure Start_File (Self : in out Ada_Lang_IO_Output_Type; File_Name : String) is
      begin   
            -- Close previous file (if exists)
            if Ada.Text_IO.Is_Open (Self.Current_File) then
               Ada.Text_IO.Close (Self.Current_File);
            end if;

            -- Open new file
            Ada.Text_IO.Create (Self.Current_File, Ada.Text_IO.Out_File, Ada.Strings.Unbounded.To_String (Self.Output_Path) & "/" & File_Name);

            Make_New_Sidebar (Self);
      end Start_File;

      procedure Put (Self : in out Ada_Lang_IO_Output_Type; Char : Character) is
      begin
         Put (Self.Current_File, Char);
      end Put;

      procedure Put (Self : in out Ada_Lang_IO_Output_Type; S : String) is
      begin
         Put (Self.Current_File, S);
      end Put;

      -- Hook to more easily allow output to file.
      procedure Put_Line (Self : in out Ada_Lang_IO_Output_Type; S : String) is
      begin
         Put_Line (Self.Current_File, S);
      end Put_Line;

      procedure New_Line (Self : in out Ada_Lang_IO_Output_Type; Count : Ada.Text_IO.Positive_Count := 1) is
      begin
         New_Line (Self.Current_File, Count);
      end New_Line;

      procedure Trace (Self : in out Ada_Lang_IO_Output_Type; S : String) is
      begin
         --  Put_Line (Self, "@@@ " & S);
         pragma Unreferenced (Self);
         pragma Unreferenced (S);
      end Trace;

      -- Outputs the current buffer in the current format.
      procedure Flush (Self : in out Ada_Lang_IO_Output_Type) is
      begin
         if not (for all X in 1 .. Ada.Strings.Unbounded.Length (Self.Buffer)
                  => Ada.Strings.Unbounded.Element (Self.Buffer, X) = ' ')
         then
            case Self.Current_Paragraph.Style is
               when Code_Block_Style =>
                  Detail.Put_Line (Self, "<CodeBlock>");
               when ARM_Output.Small
               | ARM_Output.Small_Wide_Above => null;
                  --  Detail.Put_Line (Self, ":::note");
               when others =>
                  null;
            end case;

            --  Detail.Put_Line (Self, "@" & Format_to_String (Self.Current_Format));
            Detail.Put (Self, Ada.Strings.Unbounded.To_String (Self.Buffer));

            case Self.Current_Paragraph.Style is
               when Code_Block_Style =>
                  Detail.New_Line (Self);
                  Detail.Put_Line (Self, "</CodeBlock>");
               when ARM_Output.Small
               | ARM_Output.Small_Wide_Above => null;
                  --  Detail.New_Line (Self);
                  --  Detail.Put_Line (Self, ":::");
               when others =>
                  null;
            end case;

            Detail.New_Line (Self);
         end if;

         Self.Buffer := Ada.Strings.Unbounded.Null_Unbounded_String;
      end Flush;
   end Detail;

   ----------------------------------------------------------------------------

   procedure Func (Self : in out Ada_Lang_IO_Output_Type; Name : String) is
      Converted_Name : constant Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   begin
      Put_Line ("**" & Name);

      if not Self.Call_Count.Contains (Converted_Name) then
         Self.Call_Count.Insert (Converted_Name, 0);
      end if;
      Self.Call_Count (Converted_Name) := Self.Call_Count (Converted_Name) + 1;

      -- Track the transition from the previous function to this function.
      if not Self.Transitions.Contains (Self.Last_Func) then
         Self.Transitions.Insert (Self.Last_Func, String_Sets.Empty_Set);
      end if;
      
      if not Self.Transitions (Self.Last_Func).Contains (Converted_Name) then
         Self.Transitions (Self.Last_Func).Insert (Converted_Name);
      end if;

      -- Update the trace of the last function for next time.
      Self.Last_Func := Converted_Name;
   end Func;

   procedure Prop (Property : String; Indent : Positive := 1) is
      Indent_Size : constant := 4;
      Indent_String : constant String (1 .. Indent * Indent_Size) := (others => ' ');
   begin
      pragma Unreferenced (Property);
      pragma Unreferenced (Indent_Size);
      pragma Unreferenced (Indent_String);
      --  Put_Line ("    " & Property);
   end Prop;

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
   begin
      Self.Output_Path := Ada.Strings.Unbounded.To_Unbounded_String (Output_Path);

      Ada.Text_IO.Create (Self.Current_File, Ada.Text_IO.Out_File, "Title.mdx");

      Make_New_Sidebar (Self);

      Print_Manual_Warning (Self);

      Func (Self, "Create");
      Prop ("File Prefix: " & File_Prefix);
      Prop ("Output path: " & Output_Path);
      Self.Verbose := Verbose;
   end Create;

   procedure Close (Self : in out Ada_Lang_IO_Output_Type) is
   -- Close an Self. No further output to the object is
   -- allowed after this call.
   begin
      Func (Self, "Close");

      if Self.Verbose then
         for Cursor in Self.Call_Count.Iterate loop
            Put_Line (Ada.Strings.Unbounded.To_String (String_Int_Maps.Key (Cursor)) & " : " & Natural'Image (String_Int_Maps.Element (Cursor)));
         end loop;

         for Cursor in Self.Transitions.Iterate loop
            declare
               Source : constant Ada.Strings.Unbounded.Unbounded_String := String_Set_Maps.Key (Cursor);
            begin
               for Destination in String_Set_Maps.Element (Cursor).Iterate loop
                  Put_Line (Ada.Strings.Unbounded.To_String (Source) & " -> " & Ada.Strings.Unbounded.To_String (String_Sets.Element (Destination)) & ";");
               end loop;
            end;
         end loop;
      end if;
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
      --  Func (Self, "Section");
      --  Prop ("Section Title: " & Section_Title);
      --  Prop ("Section Name:" & Section_Name);

      -- TODO: Add anchor
      pragma Unreferenced (Self);
      pragma Unreferenced (Section_Title);
      pragma Unreferenced (Section_Name);

      null;
   end Section;

   --
   -- Set the number of columns.
   -- Raises Not_Valid_Error if in a paragraph.
   --
   procedure Set_Columns (
      Self     : in out Ada_Lang_IO_Output_Type;
      Number_of_Columns : in ARM_Output.Column_Count
   ) is
   begin
      Func (Self, "Set_Columns");
      Prop ("Number of columns: " & Number_of_Columns'Image);
   end Set_Columns;

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
      Detail.Trace (Self, "Start_Paragraph: " & Paragraph_To_String (New_Paragraph));
      --  Func (Self, "Start_Paragraph");
      --  Prop ("Style:" & Style'Image);
      --  Prop ("Indent: " & Indent'Image);
      --  Prop ("Number: " & Number);
      --  Prop ("No prefix: " & No_Prefix'Image);
      --  Prop ("Space after: " & Space_After'Image);
      --  Prop ("Justification: " & Justification'Image);

      Self.Current_Paragraph := New_Paragraph;

      Self.In_Code_Block := Style in Code_Block_Style;

      if not Self.In_Code_Block then
         Detail.Append (Self, "<p>");
      end if;
   end Start_Paragraph;

   procedure End_Paragraph (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      --  Func (Self, "End_Paragraph");
      if not Self.In_Code_Block then
         Detail.Append (Self, "</p>");
      end if;
      Detail.Append (Self, Ada.Characters.Latin_1.LF);
      Detail.Flush (Self);
      Detail.Trace (Self, "End_Paragraph");

      Self.In_Code_Block := False;
   end End_Paragraph;

   -- Output a Category header (that is, "Legality Rules",
   -- "Dynamic Semantics", etc.)
   -- (Note: We did not use a enumeration here to insure that these
   -- headers are spelled the same in all output versions).
   -- Raises Not_Valid_Error if in a paragraph.
   procedure Category_Header (
      Self : in out Ada_Lang_IO_Output_Type;
      Header_Text   : String
   ) is 
   begin
      --  Func (Self, "Category_Header");
      --  Prop ("Header Text: " & Header_Text);
      Detail.New_Line (Self);
      Detail.Put_Line (Self, "#### " & Header_Text);
      Detail.New_Line (Self);
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
      File_Name : String := "AA-" & (if Clause_Number = "" and then
		(Header_Text = "Table of Contents" or else -- Ada 95 format
		 Header_Text = "Contents") then "TOC" else Clause_Number) & ".mdx";
   begin
      --  Func (Self, "Clause_Header");
      --  Prop ("Header Text:   " & Header_Text);
      --  Prop ("Level:         " & Level'Image);
      --  Prop ("Clause Number: " & Clause_Number);
      --  Prop ("Top Level Subdivision Name: " & Top_Level_Subdivision_Name'Image);

      case Level is
         when ARM_Contents.Section
         | ARM_Contents.Plain_Annex
         | ARM_Contents.Informative_Annex
         | ARM_Contents.Normative_Annex =>
            Detail.Start_File (Self, File_Name);
            
            Detail.New_Line (Self);
            Detail.Put_Line (Self, "# " & Clause_Number & " " & Header_Text);
            Detail.New_Line (Self);

            Print_Manual_Warning (Self);
            Include_React_Elements (Self);

         when ARM_Contents.Clause =>
            Detail.Start_File (Self, "AA-" & Clause_Number & ".mdx");

            Detail.New_Line (Self);
            Detail.Put_Line (Self, "# " & Clause_Number & "  " & Header_Text);
            Detail.New_Line (Self);

            Print_Manual_Warning (Self);
            Include_React_Elements (Self);

         when ARM_Contents.Subclause =>
            Detail.New_Line (Self);
            Detail.Put_Line (Self, "## " & Clause_Number & "  " & Header_Text);
            Detail.New_Line (Self);
         when ARM_Contents.Subsubclause =>
            Detail.New_Line (Self);
            Detail.Put_Line (Self, "### " & Clause_Number & "  " & Header_Text);
            Detail.New_Line (Self);
         when others =>
            null;
      end case;
   end Clause_Header;

   procedure Revised_Clause_Header
     (Self : in out Ada_Lang_IO_Output_Type; New_Header_Text : in String;
      Old_Header_Text : in String; Level : in ARM_Contents.Level_Type;
      Clause_Number : in String;
      Version : in ARM_Contents.Change_Version_Type;
      Old_Version : in ARM_Contents.Change_Version_Type;
      Top_Level_Subdivision_Name : in ARM_Output
        .Top_Level_Subdivision_Name_Kind;
      No_Page_Break : in Boolean := False) is
   begin
      Func (Self, "Revised_Clause_Header");
      Prop ("Old header text: " & Old_Header_Text);
      Prop ("Clause Number: " & Clause_Number);
      Prop ("Old Version: " & Old_Version'Image);
      Prop ("Top Level Subdivision Name" & Top_Level_Subdivision_Name'Image);
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
      Func (Self, "TOC_Marker");
      Prop ("For start: " & For_Start'Image);
   end TOC_Marker;

   procedure New_Column (Self : in out Ada_Lang_IO_Output_Type) is
   -- Output a column break.
   -- Raises Not_Valid_Error if in a paragraph, or if the number of
   -- columns is 1.
   begin
      Func (Self, "New_Column");
   end New_Column;

   procedure Start_Table
     (Self      : in out Ada_Lang_IO_Output_Type;
      Columns            : in     ARM_Output.Column_Count;
      First_Column_Width : in     ARM_Output.Column_Count;
      Last_Column_Width  : in     ARM_Output.Column_Count;
      Alignment          : in     ARM_Output.Column_Text_Alignment;
      No_Page_Break      : in     Boolean; Has_Border : in Boolean;
      Small_Text_Size    : in     Boolean;
      Header_Kind        : in     ARM_Output.Header_Kind_Type)
   is
   begin
      Func (Self, "Start_Table");
      Prop ("Columns: " & Columns'Image);
      Prop ("First Column Width: " & First_Column_Width'Image);
      Prop ("Last Column Width:  " & Last_Column_Width'Image);
      Prop ("Alignment:          " & Alignment'Image);
      Prop ("Header kind:        " & Header_Kind'Image);
   end Start_Table;

   procedure Table_Marker (
      Self : in out Ada_Lang_IO_Output_Type;
      Marker : in ARM_Output.Table_Marker_Type
   ) is
   begin
      Func (Self, "Table_Marker");
      Prop ("Marker: " & Marker'Image);
   end Table_Marker;

   -- Output a separator line. It is thin if "Is_Thin" is true.
   -- Raises Not_Valid_Error if in a paragraph.
   procedure Separator_Line
     (Self : in out Ada_Lang_IO_Output_Type; Is_Thin : Boolean := True)
   is
   begin
      Func (Self, "Separator_Line");
   end Separator_Line;

   -- Text output: These are only allowed after a Start_Paragraph and
   -- before any End_Paragraph. Raises Not_Valid_Error if not allowed.
   -- Output ordinary text.
   -- The text must end at a word break, never in the middle of a word.
   procedure Ordinary_Text
     (Self : in out Ada_Lang_IO_Output_Type; Text : in String)
   is
   begin
      --  Func (Self, "Ordinary_Text");
      --  Prop ("Text: " & Text);
      --  Detail.Put_Line (Self, Text);
      for Char of Text loop
         Detail.Append (Self, Char);
      end loop;
   end Ordinary_Text;

   procedure Ordinary_Character
     (Self : in out Ada_Lang_IO_Output_Type; Char : in Character)
   is
   begin
      --  Func (Self, "Ordinary_Character");
      --  Prop ("Char: " & Char'Image);
      Detail.Append (Self, Safe_Char (Self.In_Code_Block, Char));
   end Ordinary_Character;

   procedure Hard_Space (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      -- Just treat as a space.
      --  Func (Self, "Hard_Space");
      Ordinary_Character (Self, ' ');
   end Hard_Space;

   -- Output a line break. This does not start a new paragraph.
   -- This corresponds to a "<BR>" in HTML.
   procedure Line_Break (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      --  Func (Self, "Line_Break");
      --  Detail.New_Line (Self, 1);
      Ordinary_Character (Self, Ada.Characters.Latin_1.LF);
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
      --  Func (Self, "Index_Line_Break");
   end Index_Line_Break;

   -- Output a soft line break. This is a place (in the middle of a
   -- "word") that we allow a line break. It is usually used after
   -- underscores in long non-terminals.
   procedure Soft_Line_Break (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      -- Ignored since this is a web based format.
      pragma Unreferenced (Self);
      --  Func (Self, "Soft_Line_Break");      
   end Soft_Line_Break;

   -- Output a soft line break, with a hyphen. This is a place (in the middle of
   -- a "word") that we allow a line break. If the line break is used,
   -- a hyphen will be added to the text.
   procedure Soft_Hyphen_Break (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      -- Soft hyphens are ignored.
      pragma Unreferenced (Self);
      --  Func (Self, "Soft_Hyphen_Break");
   end Soft_Hyphen_Break;

   -- Output a tab, inserting space up to the next tab stop.
   -- Raises Not_Valid_Error if the paragraph was created with
   -- Tab_Stops = ARM_Output.NO_TABS.
   procedure Tab (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      Ordinary_Character (Self, Ada.Characters.Latin_1.HT);
      --  Func (Self, "Tab");
   end Tab;

   -- Output an special character.
   procedure Special_Character
     (Self : in out Ada_Lang_IO_Output_Type;
      Char : in ARM_Output.Special_Character_Type)
   is
   begin
      --  Func (Self, "Special_Character");
      --  Prop ("Char: " & Char'Image);
      case Char is
         when ARM_Output.EM_Dash => Ordinary_Character (Self, '-');
         when ARM_Output.Left_Double_Quote => Ordinary_Character (Self, '"');
         when ARM_Output.Right_Double_Quote => Ordinary_Character (Self, '"');
         when ARM_Output.Right_Quote => Ordinary_Character (Self, ''');
         when ARM_Output.Left_Quote => Ordinary_Character (Self, ''');
         when others => null;
         
            --  EM_Dash, -- EM (very long) dash.
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
      Func (Self, "Unicode_Character");
      Prop ("Char_Code: " & Char_Code);
   end Unicode_Character;

   -- Marks the end of a hanging item. Call only once per paragraph.
   -- Raises Not_Valid_Error if the paragraph style is not in
   -- Text_Prefixed_Style_Subtype, or if this has already been
   -- called for the current paragraph, or if the paragraph was started
   -- with No_Prefix = True.
   procedure End_Hang_Item (Self : in out Ada_Lang_IO_Output_Type) is
   begin
      Func (Self, "End_Hang_Item");
   end End_Hang_Item;

   -- Change the text format so that all of the properties are as specified.
   -- Note: Changes to these properties ought be stack-like; that is,
   -- Bold on, Italic on, Italic off, Bold off is OK; Bold on, Italic on,
   -- Bold off, Italic off should be avoided (as separate commands).
   procedure Text_Format
     (Self : in out Ada_Lang_IO_Output_Type;
      Format : in ARM_Output.Format_Type)
   is
      use type ARM_Output.Font_Family_Type;
      use type ARM_Output.Format_Type;
   begin
      if not Self.In_Code_Block then
         -- Turn off any changed formatting before turning on any formatting
         -- and also turn off formatting in the inverse order as added
         -- formatting to keep stack-like behavior when multiple states change
         -- at once.
         if Format /= Self.Current_Format then
            if Format.Font /= ARM_Output.Swiss and then Self.Current_Format.Font = ARM_Output.Swiss then
               Detail.Append (Self, "</code>");  
            end if;

            if Format.Italic /= Self.Current_Format.Italic and then not Format.Italic then
               Detail.Append (Self, "</em>");
            end if;

            if Format.Bold /= Self.Current_Format.Bold and then not Format.Bold then
               Detail.Append (Self, "</strong>");
            end if;

            if Format.Bold /= Self.Current_Format.Bold and then Format.Bold then
               Detail.Append (Self, "<strong>");
            end if;

            if Format.Italic /= Self.Current_Format.Italic and then Format.Italic then
               Detail.Append (Self, "<em>");
            end if;

            if Format.Font = ARM_Output.Swiss and then Self.Current_Format.Font /= ARM_Output.Swiss then
               Detail.Append (Self, "<code>");  
            end if;
         end if;
      end if;

      --  Func (Self, "Text_Format");
      --  Prop ("Format: ");
      --  Prop ("Bold: " & Format.Bold'Image, 2);
      --  Prop ("Italic: " & Format.Italic'Image, 2);
      --  Prop ("Font: " & Format.Font'Image, 2);
      --  Prop ("Size: " & Format.Size'Image, 2);
      --  Prop ("Color: " & Format.Color'Image, 2);
      --  Prop ("Change: " & Format.Change'Image, 2);
      --  Prop ("Version: " & Format.Version'Image, 2);
      --  Prop ("Added_Version: " & Format.Added_Version'Image, 2);
      --  Prop ("Location: " & Format.Location'Image, 2);

      --  Detail.Trace (Self, Format_To_String (Format));

      --  Should flush the last bit since the last text format change...
      --  but only when NOT writing code samples, since text formats
      --  are used in the code samples...
      --
      --  Detail.Flush (Self);
      
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
      --  Func (Self, "Clause_Reference");
      --  Prop ("Clause Number: " & Clause_Number);

      -- Ignore this by consuming the buffer.
      --  Self.Buffer := Ada.Strings.Unbounded.Null_Unbounded_String;

      -- todo: make a link instead
      Detail.Append (Self, Text);
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
      --  Func (Self, "Index_Target");
      --  Prop ("Index Key: " & Index_Key'Image);
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

      --  Func (Self, "Index_Reference");
      --  Prop ("Text: " & Text);
      --  Prop ("Index_Key: " & Index_Key'Image);

      Detail.Append (Self, Text);
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
      --  Func (Self, "DR_Reference");
      --  Prop ("DR Number: " & DR_Number);

      Detail.Append (Self, Text);
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
      --  Func (Self, "AI_Reference");
      --  Prop ("Text: " & Text);
      --  Prop ("AI_Number: " & AI_Number);
      --  Detail.Append (Self, (if Self.In_Code_Block then JSX_Wrap (Text) else Text));
      Detail.Append (Self, JSX_Wrap (Text));
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
      --  Func (Self, "Local_Target");
      --  Prop ("Text: " & Text);
      --  Prop ("Target: " & Target);

      Detail.Append (Self, 
         "<a id=""" & Target & """>"
         & Text
         & "</a>");
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
      --  Func (Self, "Local_Link");
      --  Prop ("Text: " & Text);
      --  Prop ("Target: " & Target);
      --  Prop ("Clause Number: " & Clause_Number);

      Detail.Append (Self, Make_Link (Text, Make_Clause_File_Name (Clause_Number) & "#" & Target, Self.In_Code_Block));
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
      --  Func (Self, "Local_Link_Start");
      --  Prop ("Target: " & Target);
      --  Prop ("Clause Number: " & Clause_Number);

      -- todo: start link
      if Self.In_Code_Block then
         Detail.Append (Self, "<a href=""" & Make_Clause_File_Name (Clause_Number) & "#" & Target & """>");
      else
         Detail.Append (Self, "[");
      end if;
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
      --  Func (Self, "Local_Link_End");
      --  Prop ("Target: " & Target);
      --  Prop ("Clause Number: " & Clause_Number);

      Detail.Append (Self, "](" & Make_Clause_File_Name (Clause_Number) & "#" & Target & ")");
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
      Func (Self, "URL_Link");
      Prop ("Text: " & Text);
      Prop ("URL: " & URL);

      Detail.Append (Self, Make_Link (Text, URL, Self.In_Code_Block));
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
      Func (Self, "Picture");
      Prop ("Name: " & Name);
      Prop ("Description: " & Descr);
      Prop ("Alignment: " & Alignment'Image);
      Prop ("Height: " & Height'Image);
      Prop ("Width: " & Width'Image);
      Prop ("Border: " & Border'Image);
   end Picture;

end ARM_Ada_Lang_IO;
