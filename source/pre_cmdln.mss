@comment{ $Source: e:\\cvsroot/ARM/Source/pre_cmdln.mss,v $ }
@comment{ $Revision: 1.20 $ $Date: 2000/08/31 04:56:10 $ $Author: Randy $ }
@Part(predefcmdln, Root="ada.mss")

@Comment{$Date: 2000/08/31 04:56:10 $}
@LabeledClause{The Package Command_Line}
@begin{Intro}
The package Command_Line allows a program to obtain the values of its
arguments and to set the exit status code to be returned on normal termination.
@ImplDef{The meaning of Argument_Count, Argument, and Command_Name.}
@end{Intro}

@begin{StaticSem}
@Leading@Keepnext@;The library package Ada.Command_Line has the following declaration:
@begin{Example}
@key[package] Ada.Command_Line @key[is]@ChildUnit{Parent=[Ada],Child=[Command_Line]}
  @key[pragma] Preelaborate(Command_Line);

  @key[function] @AdaSubDefn{Argument_Count} @key[return] Natural;

  @key[function] @AdaSubDefn{Argument} (Number : @key[in] Positive) @key[return] String;

  @key[function] @AdaSubDefn{Command_Name} @key[return] String;

  @key[type] @AdaTypeDefn{Exit_Status} @key[is] @RI{implementation-defined integer type};

  @AdaDefn{Success} : @key[constant] Exit_Status;
  @AdaDefn{Failure} : @key[constant] Exit_Status;

  @key[procedure] @AdaSubDefn{Set_Exit_Status} (Code : @key[in] Exit_Status);

@key[private]
  ... -- @RI{not specified by the language}
@key[end] Ada.Command_Line;
@comment{Blank line}
@end{example}
@begin{DescribeCode}
@begin{Example}@Keepnext
@key[function] Argument_Count @key[return] Natural;
@end{Example}
@Trailing@;If the external execution environment supports passing arguments to
a program, then
Argument_Count returns
the number of arguments passed to the program
invoking the function. Otherwise it returns 0.
The meaning of @lquotes@;number of arguments@rquotes@; is implementation defined.

@begin{Example}@Keepnext
@key[function] Argument (Number : @key[in] Positive) @key[return] String;
@end{Example}
@Trailing@;If the external execution environment supports passing arguments to
a program, then
Argument returns an implementation-defined value corresponding to
the argument at relative position Number.
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
If Number is outside the range 1..Argument_Count, then
Constraint_Error is propagated.
@begin{Ramification}
If the external execution environment does not support
passing arguments to a program, then Argument(N) for any N will
raise Constraint_Error, since Argument_Count is 0.@end{ramification}

@begin{Example}@Keepnext
@key[function] Command_Name @key[return] String;
@end{Example}
@Trailing@;If the external execution environment supports passing arguments to
a program, then
Command_Name returns an implementation-defined value corresponding to
the name of the command invoking the program;
otherwise Command_Name returns the null string.

@Comment{This is missing; leading the following paragraph glued to "Command_Name"}
@begin{Example}@Keepnext
@ChgRef{Version=[1],Kind=[Added]}
@Chg{New=[@key[type] @AdaTypeDefn{Exit_Status} @key[is] @RI{implementation-defined integer type};],Old=[]}
@end{Example}
@Trailing@;
The type Exit_Status represents the range of exit
status values supported by the external execution environment.
The constants Success and Failure correspond to success and failure,
respectively.

@begin{Example}@Keepnext
@key[procedure] Set_Exit_Status (Code : @key[in] Exit_Status);
@end{Example}
If the external execution environment supports returning an exit status
from a program, then Set_Exit_Status sets Code as the status. Normal
termination of a program returns as the exit status the value most
recently set by Set_Exit_Status, or, if no such value has been set, then the
value Success. If a program terminates abnormally, the status set by
Set_Exit_Status is ignored, and an implementation-defined exit status value
is set.

@ChgNote{An incorrect index entry; presentation AI-00005}
@Chg{New=[],Old=[@PDefn{unspecified}]}
If the external execution environment does not support returning
an exit value from a program, then Set_Exit_Status does nothing.
@end{DescribeCode}
@end{StaticSem}

@begin{ImplPerm}
An alternative declaration is allowed
for package Command_Line if different functionality is appropriate
for the external execution environment.
@end{ImplPerm}


@begin{Notes}
Argument_Count, Argument, and Command_Name
correspond to the C language's argc, argv[n] (for n>0) and argv[0],
respectively.
@begin{Ramification}

The correspondence of Argument_Count to argc is not direct @em
argc would be one more than Argument_Count, since the argc count
includes the command name,
whereas Argument_Count does not.

@end{Ramification}
@end{Notes}

@begin{Extend83}
@Defn{extensions to Ada 83}
This clause is new in Ada 95.
@end{Extend83}