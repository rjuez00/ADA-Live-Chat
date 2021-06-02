-- autor: Adrián Lanza

with Text_IO;
with Chat_Messages;
with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Exceptions;
with Handlers;



procedure Chat_Client_2 is
    
    package ACL renames Ada.Command_Line;
    package LLU renames Lower_Layer_UDP;
    package M renames Chat_Messages;
    package ASU renames Ada.Strings.Unbounded;
    use type M.Message_Type;

    Server_Disconnected: exception;
    Access_Denied: exception;

    procedure Log_In(   Server_EP: in LLU.End_Point_Type;
                        Nick: in ASU.Unbounded_String;
                        Buffer: access LLU.Buffer_Type;
                        Client_EP: out LLU.End_Point_Type) is
        
        Welcome_EP: LLU.End_Point_Type;
        Receivent: Boolean;

        Message_Typ: M.Message_type;
        Accepted: Boolean;
    begin
        LLU.Bind_Any(Welcome_EP);
        LLU.Bind_Any(Client_EP, Handlers.Client_Receiver'Access);

        LLU.Reset(Buffer.all);

        M.Message_type'Output(Buffer, M.Init);
        LLU.End_Point_Type'Output(Buffer, Welcome_EP);
        LLU.End_Point_Type'Output(Buffer, CLient_EP);
        ASU.Unbounded_String'Output(Buffer, Nick);

        LLU.Send(Server_EP, Buffer);
        LLU.Reset(Buffer.all);
        LLU.Receive(Welcome_EP, Buffer, 10.0, Receivent);
        if Receivent then
            raise Server_Disconnected;
        end if;

        Message_Typ := M.Message_type'Input(Buffer);
        Accepted := Boolean'Input(Buffer);

        if Accepted then
            Text_IO.Put_Line ("Mini-Chat v2.0:  Welcome " & ASU.To_String (Nick));
        else
            raise Access_Denied;
        end if;
    end Log_In;




    procedure Loop_Chat(    Server_EP: in LLU.End_Point_Type;
                            Client_EP: in LLU.End_Point_Type;
                            Nick: in ASU.Unbounded_String;
                            Buffer: access LLU.BUffer_Type) is
        Input: ASU.Unbounded_String;
    begin
        loop
            Text_IO.Put(">> ");
            Input:= ASU.To_Unbounded_String (Text_IO.Get_line);

            if ASU.To_String(Input) = ".quit" then
                exit;
            else
                LLU.Reset(Buffer.all);
                
                M.Message_type'Output(Buffer, M.Writer);
                LLU.End_Point_Type'Output(Buffer, Client_EP);
                ASU.Unbounded_String'Output(Buffer, Nick);
                ASU.Unbounded_String'Output(Buffer, Input);
                LLU.Send(Server_EP, Buffer);
            end if;

            

        end loop;
        LLU.Reset(Buffer.all);

        M.Message_Type'Output(Buffer, M.Logout);
        LLU.End_Point_Type'Output(Buffer, Client_EP);
        ASU.Unbounded_String'Output(Buffer, Nick);
        LLU.Send(Server_EP, Buffer);

        LLU.Finalize;

    end Loop_Chat;






    Server_Name: ASU.Unbounded_String;
    Port: Integer;
    Nick: ASU.Unbounded_String;
    Server_EP: LLU.End_Point_Type;
    Client_EP: LLU.End_Point_Type;
    Buffer: aliased LLU.Buffer_Type(4096);
begin

    if ACL.Argument_Count /= 3 then
        Text_IO.Put_Line("Usage: ./chat_client_2 <server hostname> <port> <nick>");
        LLU.Finalize;
        return;
    end if;
    Server_Name := ASU.To_Unbounded_String (ACL.Argument (1));
    Port := Integer'Value(ACL.Argument (2));
    Nick := ASU.To_Unbounded_String (ACL.Argument (3));
    if ASU.To_String (Nick) = "server" then
        Text_IO.Put_Line ("server nick is reserved");
        LLU.Finalize;
        return;
    end if;
    Server_EP:= LLU.Build(LLU.To_IP(ASU.To_String (Server_Name)), Port);
    
    Log_In (Server_EP, Nick, Buffer'access, Client_EP);
    Loop_Chat(Server_EP, Client_EP, Nick, Buffer'access);
    
    


    exception
        when Server_Disconnected =>
            Text_IO.Put_Line("Server unreachable");
            LLU.Finalize;
        when Access_Denied =>
            Text_IO.Put_Line("Mini-Chat v2.0: IGNORED new user " & ASU.To_String (Nick) & ", nick already used");
            LLU.Finalize;
        when Ex:others =>
            Text_IO.Put_Line(Ada.Exceptions.Exception_Name(Ex) &
                        ": " & Ada.Exceptions.Exception_Message(Ex));
            LLU.Finalize;
end Chat_Client_2;
