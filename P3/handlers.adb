-- autor: Adrián Lanza

with Lower_Layer_UDP;
with Chat_Messages;
with Text_IO;
with Ada.Strings.Unbounded;
with Server_Database;

package body Handlers is
    package M renames Chat_Messages;
    package ASU renames Ada.Strings.Unbounded;

    procedure Client_Receiver ( From: in LLU.End_Point_Type;
                                To: in LLU.End_Point_Type;
                                Buffer: access LLU.Buffer_Type) is
        Nick: ASU.Unbounded_String;
        Output: ASU.Unbounded_String;
        Message_Type: M.Message_Type;
        
    begin
        Message_Type:= M.Message_Type'Input(Buffer);
        Nick := ASU.Unbounded_String'Input(Buffer);
        Output := ASU.Unbounded_String'Input(Buffer);
        Text_io.Put_Line (""); -- es un newline
        Text_IO.Put_Line(ASU.To_String(Nick) & ": " & ASU.To_String(Output));
        Text_IO.Put (">> "); -- se necesita para que cuadre con los requisitos de output
    end Client_Receiver;



    procedure Server_Receiver   (From: in  LLU.End_Point_Type;
                                 To: in LLU.End_Point_Type;
                                 Buffer: access LLU.Buffer_Type) is
        Message_Type: M.Message_Type;
    begin
        Message_Type := M.Message_type'Input(Buffer);
        case Message_Type is
            when M.Writer =>
                Server_Database.Receive_Message (Buffer);
            when M.Logout =>
                Server_Database.Client_LogOut(Buffer);
            when M.Init =>
                Server_Database.Register (Buffer);
            when others =>
                Null;
        end case; 
    end Server_Receiver;
    

    
end Handlers;