with ARM_Contents;
with ARM_Output;

with Ada.Strings.Unbounded;
with Ada.Text_IO;

-- A formatter for outputting MDX (Markdown + JSX) files used for adding to
-- the ada-lang.io website.
--
-- This opens up syntax past that of Markdown, especially to include <CodeBlock>
-- for syntax formatting.
package ARM_Ada_Lang_IO is

   type Ada_Lang_IO_Output_Type is new ARM_Output.Output_Type with private;

   -- Create an Self for a document.
   -- The prefix of the output file names is File_Prefix - this
   -- should be no more then 5 characters allowed in file names.
   -- The result files will be written to Output_Path.
   -- The title of the document is Title.
   procedure Create(
      Self : in out Ada_Lang_IO_Output_Type;
      File_Prefix   : in String;
      Output_Path   : in String;
      Title         : in String := "";
      Verbose       : in Boolean := True);

   -- Close an Self. No further output to the object is
   -- allowed after this call.
   procedure Close (Self : in out Ada_Lang_IO_Output_Type);

   -- Start a new section. The title is Section_Title (this is
   -- intended for humans). The name is Section_Name (this is
   -- intended to be suitable to be a portion of a file name).
   procedure Section
     (Self : in out Ada_Lang_IO_Output_Type; Section_Title : in String;
      Section_Name  : in     String);

   -- Set the number of columns.
   -- Raises Not_Valid_Error if in a paragraph.
   procedure Set_Columns
     (Self     : in out Ada_Lang_IO_Output_Type;
      Number_of_Columns : in     ARM_Output.Column_Count);

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
     (Self          : in out Ada_Lang_IO_Output_Type;
      Style         : in     ARM_Output.Paragraph_Style_Type;
      Indent        : in ARM_Output.Paragraph_Indent_Type; Number : in String;
      No_Prefix     : in     Boolean                       := False;
      Tab_Stops     : in     ARM_Output.Tab_Info := ARM_Output.NO_TABS;
      No_Breaks     : in Boolean := False; Keep_with_Next : in Boolean := False;
      Space_After   : in     ARM_Output.Space_After_Type := ARM_Output.Normal;
      Justification : in ARM_Output.Justification_Type := ARM_Output.Default);

   procedure End_Paragraph (Self : in out Ada_Lang_IO_Output_Type);
   -- End a paragraph.

   procedure Category_Header
     (Self : in out Ada_Lang_IO_Output_Type; Header_Text : String);
   -- Output a Category header (that is, "Legality Rules",
   -- "Dynamic Semantics", etc.)
   -- (Note: We did not use a enumeration here to insure that these
   -- headers are spelled the same in all output versions).
   -- Raises Not_Valid_Error if in a paragraph.

   -- Output a Clause header. The level of the header is specified
   -- in Level. The Clause Number is as specified; the top-level (and
   -- other) subdivision names are as specified. These should appear in
   -- the table of contents.
   -- For hyperlinked formats, this should generate a link target.
   -- If No_Page_Break is True, suppress any page breaks.
   -- Raises Not_Valid_Error if in a paragraph.
   procedure Clause_Header (
      Self                       : in out Ada_Lang_IO_Output_Type;
      Header_Text                : in String;
      Level                      : in ARM_Contents.Level_Type;
      Clause_Number              : in String;
      Top_Level_Subdivision_Name : in ARM_Output.Top_Level_Subdivision_Name_Kind;
      No_Page_Break : in Boolean := False);

   -- Output a revised clause header. Both the original and new text will
   -- be output. The level of the header is specified in Level. The Clause
   -- Number is as specified; the top-level (and other) subdivision names
   -- are as specified. These should appear in the table of contents.
   -- For hyperlinked formats, this should generate a link target.
   -- Version is the insertion version of the new text; Old_Version is
   -- the insertion version of the old text.
   -- If No_Page_Break is True, suppress any page breaks.
   -- Raises Not_Valid_Error if in a paragraph.
   procedure Revised_Clause_Header
     (Self : in out Ada_Lang_IO_Output_Type;
      New_Header_Text : in String;
      Old_Header_Text : in String;
      Level : in ARM_Contents.Level_Type;
      Clause_Number : in String;
      Version : in ARM_Contents.Change_Version_Type;
      Old_Version : in ARM_Contents.Change_Version_Type;
      Top_Level_Subdivision_Name : in ARM_Output.Top_Level_Subdivision_Name_Kind;
      No_Page_Break : in Boolean := False);

   procedure TOC_Marker
     (Self : in out Ada_Lang_IO_Output_Type; For_Start : in Boolean);
   -- Mark the start (if For_Start is True) or end (if For_Start is
   -- False) of the table of contents data. Output objects that
   -- auto-generate the table of contents can use this to do needed
   -- actions.

   procedure New_Page
     (Self : in out Ada_Lang_IO_Output_Type;
      Kind          :        ARM_Output.Page_Kind_Type := ARM_Output.Any_Page) is null;
   -- Output a page break.
   -- Note that this has no effect on non-printing formats.
   -- Any_Page breaks to the top of the next page (whatever it is);
   -- Odd_Page_Only breaks to the top of the odd-numbered page;
   -- Soft_Page allows a page break but does not force one (use in
   -- "No_Breaks" paragraphs.)
   -- Raises Not_Valid_Error if in a paragraph if Kind = Any_Page or
   -- Odd_Page, and if not in a paragraph if Kind = Soft_Page.

   procedure New_Column (Self : in out Ada_Lang_IO_Output_Type);
   -- Output a column break.
   -- Raises Not_Valid_Error if in a paragraph, or if the number of
   -- columns is 1.

   procedure Separator_Line
     (Self : in out Ada_Lang_IO_Output_Type; Is_Thin : Boolean := True);
   -- Output a separator line. It is thin if "Is_Thin" is true.
   -- Raises Not_Valid_Error if in a paragraph.

   procedure Start_Table
     (Self      : in out Ada_Lang_IO_Output_Type;
      Columns            : in     ARM_Output.Column_Count;
      First_Column_Width : in     ARM_Output.Column_Count;
      Last_Column_Width  : in     ARM_Output.Column_Count;
      Alignment          : in     ARM_Output.Column_Text_Alignment;
      No_Page_Break      : in     Boolean; Has_Border : in Boolean;
      Small_Text_Size    : in     Boolean;
      Header_Kind        : in     ARM_Output.Header_Kind_Type);
   -- Starts a table. The number of columns is Columns; the first
   -- column has First_Column_Width times the normal column width, and
   -- the last column has Last_Column_Width times the normal column width.
   -- Alignment is the horizontal text alignment within the columns.
   -- No_Page_Break should be True to keep the table intact on a single
   -- page; False to allow it to be split across pages.
   -- Has_Border should be true if a border is desired, false otherwise.
   -- Small_Text_Size means that the contents will have the AARM size;
   -- otherwise it will have the normal size.
   -- Header_Kind determines whether the table has headers.
   -- This command starts a paragraph; the entire table is a single
   -- paragraph. Text will be considered part of the caption until the
   -- next table marker call.
   -- Raises Not_Valid_Error if in a paragraph.

   procedure Table_Marker
     (Self : in out Ada_Lang_IO_Output_Type;
      Marker        : in     ARM_Output.Table_Marker_Type);
   -- Marks the end of an entity in a table.
   -- If Marker is End_Caption, the table caption ends and the
   --      future text is part of the table header.
   -- If Marker is End_Header, the table header ends and the
   --      future text is part of the table body.
   -- If Marker is End_Row, a row in the table is completed, and another
   --      row started.
   -- If Marker is End_Item, an item in the table header or body is ended,
   --      and another started.
   -- If Marker is End_Table, the entire table is finished.
   -- Raises Not_Valid_Error if not in a table.

   -- Text output: These are only allowed after a Start_Paragraph and
   -- before any End_Paragraph. Raises Not_Valid_Error if not allowed.

   procedure Ordinary_Text
     (Self : in out Ada_Lang_IO_Output_Type; Text : in String);
   -- Output ordinary text.
   -- The text must end at a word break, never in the middle of a word.

   procedure Ordinary_Character
     (Self : in out Ada_Lang_IO_Output_Type; Char : in Character);
   -- Output an ordinary character.
   -- Spaces will be used to break lines as needed.

   procedure Hard_Space (Self : in out Ada_Lang_IO_Output_Type);
   -- Output a hard space. No line break should happen at a hard space.

   procedure Line_Break (Self : in out Ada_Lang_IO_Output_Type);
   -- Output a line break. This does not start a new paragraph.
   -- This corresponds to a "<BR>" in HTML.

   procedure Index_Line_Break
     (Self        : in out Ada_Lang_IO_Output_Type;
      Clear_Keep_with_Next : in     Boolean);
   -- Output a line break for the index. This does not start a new
   -- paragraph in terms of spacing. This corresponds to a "<BR>"
   -- in HTML. If Clear_Keep_with_Next is true, insure that the next
   -- line does not require the following line to stay with it.

   procedure Soft_Line_Break (Self : in out Ada_Lang_IO_Output_Type);
   -- Output a soft line break. This is a place (in the middle of a
   -- "word") that we allow a line break. It is usually used after
   -- underscores in long non-terminals.

   procedure Soft_Hyphen_Break (Self : in out Ada_Lang_IO_Output_Type);
-- Output a soft line break, with a hyphen. This is a place (in the middle of
-- a "word") that we allow a line break. If the line break is used,
-- a hyphen will be added to the text.

   procedure Tab (Self : in out Ada_Lang_IO_Output_Type);
   -- Output a tab, inserting space up to the next tab stop.
   -- Raises Not_Valid_Error if the paragraph was created with
   -- Tab_Stops = ARM_Output.NO_TABS.

   procedure Special_Character
     (Self : in out Ada_Lang_IO_Output_Type;
      Char          : in     ARM_Output.Special_Character_Type);
   -- Output an special character.

   procedure Unicode_Character
     (Self : in out Ada_Lang_IO_Output_Type;
      Char          : in     ARM_Output.Unicode_Type);
   -- Output a Unicode character, with code position Char.

   procedure End_Hang_Item (Self : in out Ada_Lang_IO_Output_Type);
   -- Marks the end of a hanging item. Call only once per paragraph.
   -- Raises Not_Valid_Error if the paragraph style is not in
   -- Text_Prefixed_Style_Subtype, or if this has already been
   -- called for the current paragraph, or if the paragraph was started
   -- with No_Prefix = True.

   procedure Text_Format
     (Self : in out Ada_Lang_IO_Output_Type;
      Format        : in     ARM_Output.Format_Type);
   -- Change the text format so that all of the properties are as specified.
   -- Note: Changes to these properties ought be stack-like; that is,
   -- Bold on, Italic on, Italic off, Bold off is OK; Bold on, Italic on,
   -- Bold off, Italic off should be avoided (as separate commands).

   procedure Clause_Reference
     (Self : in out Ada_Lang_IO_Output_Type; Text : in String;
      Clause_Number : in     String);
   -- Generate a reference to a clause in the standard. The text of
   -- the reference is "Text", and the number of the clause is
   -- Clause_Number. For hyperlinked formats, this should generate
   -- a link; for other formats, the text alone is generated.

   procedure Index_Target
     (Self : in out Ada_Lang_IO_Output_Type; Index_Key : in Natural);
   -- Generate a index target. This marks the location where an index
   -- reference occurs. Index_Key names the index item involved.
   -- For hyperlinked formats, this should generate a link target;
   -- for other formats, nothing is generated.

   procedure Index_Reference
     (Self : in out Ada_Lang_IO_Output_Type; Text : in String;
      Index_Key     : in     Natural; Clause_Number : in String);
   -- Generate a reference to an index target in the standard. The text
   -- of the reference is "Text", and Index_Key and Clause_Number denotes
   -- the target. For hyperlinked formats, this should generate
   -- a link; for other formats, the text alone is generated.

   procedure DR_Reference
     (Self : in out Ada_Lang_IO_Output_Type; Text : in String;
      DR_Number     : in     String);
   -- Generate a reference to an DR (Defect Report) from the standard. The text
   -- of the reference is "Text", and DR_Number denotes
   -- the target. For hyperlinked formats, this should generate
   -- a link; for other formats, the text alone is generated.

   procedure AI_Reference
     (Self : in out Ada_Lang_IO_Output_Type; Text : in String;
      AI_Number     : in     String);
   -- Generate a reference to an AI from the standard. The text
   -- of the reference is "Text", and AI_Number denotes
   -- the target (in unfolded format). For hyperlinked formats, this should
   -- generate a link; for other formats, the text alone is generated.

   procedure Local_Target
     (Self : in out Ada_Lang_IO_Output_Type; Text : in String;
      Target        : in     String);
   -- Generate a local target. This marks the potential target of local
   -- links identified by "Target". Text is the text of the target.
   -- For hyperlinked formats, this should generate a link target;
   -- for other formats, only the text is generated.

   -- Generate a local link to the target and clause given.
   -- Text is the text of the link.
   -- For hyperlinked formats, this should generate a link;
   -- for other formats, only the text is generated.
   procedure Local_Link
     (Self : in out Ada_Lang_IO_Output_Type;
      Text : in String;
      Target        : in     String;
      Clause_Number : in String);

   -- Generate a local link to the target and clause given.
   -- The link will surround text until Local_Link_End is called.
   -- Local_Link_End must be called before this routine can be used again.
   -- For hyperlinked formats, this should generate a link;
   -- for other formats, only the text is generated.
   procedure Local_Link_Start
     (Self : in out Ada_Lang_IO_Output_Type;
      Target : in String;
      Clause_Number : in String);

   procedure Local_Link_End
     (Self : in out Ada_Lang_IO_Output_Type;
      Target : in String;
      Clause_Number : in String);
   -- End a local link for the target and clause given.
   -- This must be in the same paragraph as the Local_Link_Start.
   -- For hyperlinked formats, this should generate a link;
   -- for other formats, only the text is generated.

   -- Generate a link to the URL given.
   -- Text is the text of the link.
   -- For hyperlinked formats, this should generate a link;
   -- for other formats, only the text is generated.
   procedure URL_Link(
      Self : in out Ada_Lang_IO_Output_Type;
      Text : in String;
      URL  : in String;
      All_Formats : in Boolean);

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
     Name : in String;
      Descr         : in String;
      Alignment : in ARM_Output.Picture_Alignment;
      Height, Width : in     Natural;
      Border : in ARM_Output.Border_Kind);

private

   type Paragraph_Styling is record
      Style          : ARM_Output.Paragraph_Style_Type;
      Indent         : ARM_Output.Paragraph_Indent_Type;
      Number         : Ada.Strings.Unbounded.Unbounded_String;
      No_Prefix      : Boolean := False;
      Tab_Stops      : ARM_Output.Tab_Info := ARM_Output.NO_TABS;
      No_Breaks      : Boolean := False;
      Keep_with_Next : Boolean := False;
      Space_After    : ARM_Output.Space_After_Type := ARM_Output.Normal;
      Justification  : ARM_Output.Justification_Type := ARM_Output.Default;
   end record;

   type Admonition_Type is (
      Note,
      Reason,
      Discussion,
      Proof,
      Ramification,
      Implementation_Advice,
      Implementation_Defined,
      Implementation_Note,
      Correction,
      Glossary_Entry
   );

   -- These are the texts seen by the formatter.
   Admonition_Texts : constant array (Admonition_Type) of access String := (
      new String'("Note: "),
      new String'("Reason: "),
      new String'("Ramification: "),
      new String'("Discussion: "),
      new String'("Proof: "),

      new String'("Implementation Advice"),
      new String'("Implementation defined: "),
      new String'("Implementation Note: "),

      new String'("Correction:"),
      new String'("Glossary entry: ")
   );

   Admonition_Output : constant array (Admonition_Type) of access String := (
      new String'("note"),
      new String'("reason"),
      new String'("ramification"),
      new String'("discussion"),
      new String'("proof"),

      new String'("implementation-advice"),
      new String'("implementation-defined"),
      new String'("implementation-note"),

      new String'("correction"),
      new String'("glossary-entry")
   );

   type Ada_Lang_IO_Output_Type is new ARM_Output.Output_Type with record
      File_Prefix : Ada.Strings.Unbounded.Unbounded_String;
      Current_File : Ada.Text_IO.File_Type;
      Output_Path : Ada.Strings.Unbounded.Unbounded_String;

      -- The current file step being worked on.
      Current_File_Stem : Ada.Strings.Unbounded.Unbounded_String;

      -- The next index in the sidebar of the menu to be used.
      Next_Sidebar_Position : Integer := 0;

      Buffer : Ada.Strings.Unbounded.Unbounded_String;
      Current_Format : ARM_Output.Format_Type := (others => <>);
      Current_Subclause : Ada.Strings.Unbounded.Unbounded_String;
      Current_Paragraph : Paragraph_Styling;
      Paragraph_Number : Ada.Strings.Unbounded.Unbounded_String;
      Verbose : Boolean := True;

      -- Paragraph styles like code blocks get terminated when they need to be
      -- merged into cohesive wholes.  Track whether the current paragraph is
      -- mergable and also if the current paragraph is a continuation of the
      -- previous paragraph, to avoid printing start tags.
      Mergable_Paragraph : Boolean := False;
      Being_Merged : Boolean := False;
      Last_Was_AI_Reference : Boolean := False;

      -- Need to emit JSX style HTML tags when in a block like an admonition
      -- or a code block.
      -- Also additional characters might need to be escaped, like { and }
      In_Block_Tag : Boolean := False;
      Admonition_Format : Admonition_Type := Note;

      -- Front matter sections aren't in subfolders.
      In_Front_Matter : Boolean := False;
   end record;

end ARM_Ada_Lang_IO;
