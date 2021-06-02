with Text_IO;
with Lower_Layer_UDP;
with Ada.Command_Line;
with Ada.Exceptions;
with Handlers;
with Ada.Strings.Unbounded;
with Server_Database;
with Chat_Messages;
with Ada.Calendar;
with Gnat.Calendar.Time_IO;


package body Server_Database is
    package SD renames Server_Database;
    package M renames Chat_Messages;
    package ACL renames Ada.Command_Line;

    use type LLU.End_Point_Type;
    use type Ada.Calendar.Time;
    
    procedure Register (Buffer: access LLU.Buffer_Type) is
        Welcome_EP: LLU.End_Point_Type;
        Client_EP: LLU.End_Point_Type;
        Nick: ASU.Unbounded_String;
        
        In_System: Boolean := False;
        Client: SD.Client;
        Server_String: ASU.Unbounded_String := ASU.To_Unbounded_String ("server");
    begin
        Welcome_EP := LLU.End_Point_Type'Input(Buffer);
        Client_EP := LLU.End_Point_Type'Input(Buffer);
        Nick := ASU.Unbounded_String'Input(Buffer);
        Text_IO.Put("INIT received from " & ASU.To_String(Nick) & ": ");

        SD.Active_Type_Map.Get (SD.Active_Clients,
                                Nick,
                                Client,
                                In_System);
        if In_System = False then
            Text_IO.Put_line("ACCEPTED");
            if SD.Active_Type_Map.Map_Length(SD.Active_Clients) = Integer'Value(ACL.Argument(2)) then
                Expell_Oldest (Buffer);
            end if;

            SD.Active_Type_Map.Put(SD.Active_Clients, Nick, (Client_EP, Ada.Calendar.Clock));
            Send_All(Buffer, Nick, Server_String, ASU.To_Unbounded_String (ASU.To_String(Nick) & " joins the chat"));

            LLU.Reset(Buffer.all);
            M.Message_Type'Output(Buffer, M.Welcome);
            Boolean'Output(Buffer, True);
            LLU.Send(Welcome_EP, Buffer);
        else 
            Text_IO.Put_Line("IGNORED. nick already used.");
            LLU.Reset(Buffer.all);
            M.Message_Type'Output(Buffer, M.Welcome);
            Boolean'Output(Buffer, False);
            LLU.Send(Welcome_EP, Buffer);
        end if;
    end Register;


    procedure Expell_Oldest (Buffer: access LLU.Buffer_Type) is
        Traverse: SD.Active_Type_Map.Cursor := SD.Active_Type_Map.First(SD.Active_Clients);
        Nick_Expell: ASU.Unbounded_String := SD.Active_Type_Map.Element (Traverse).Key;
        Oldest: Ada.Calendar.Time := SD.Active_Type_Map.Element (Traverse).Value.LastMessage;
        Success : Boolean;
    begin

        SD.Active_Type_Map.Next(Traverse);
        while SD.Active_Type_Map.Has_Element (Traverse) = True loop
           if SD.Active_Type_Map.Element (Traverse).Value.LastMessage < Oldest then
                Nick_Expell := SD.Active_Type_Map.Element (Traverse).Key;
                Oldest := SD.Active_Type_Map.Element (Traverse).Value.LastMessage;
           end if;
           SD.Active_Type_Map.Next(Traverse);
        end loop;
        LLU.Reset(Buffer.all);
        Send_All(Buffer, ASU.To_Unbounded_String(""), ASU.To_Unbounded_String ("server"), ASU.To_Unbounded_String (ASU.To_String(Nick_Expell) & " banned for being idle for to long"));     
        SD.Active_Type_Map.Delete (SD.Active_Clients, Nick_Expell, Success);
        if Success = False then
            Text_IO.Put_Line ("ERROR WHEN DELETING INACTIVE CLIENT");
        end if;
        if SD.Inactive_Type_Map.Map_Length(SD.Inactive_Clients) > 149 then
            Text_IO.Put_Line("Inactive Client List is full");
        else
            SD.Inactive_Type_Map.Put (SD.Inactive_Clients, Nick_Expell, Oldest);
        end if;
    end Expell_Oldest;


    procedure Send_All (Buffer: access LLU.Buffer_Type; 
                        Nick_Exception: ASU.Unbounded_String; 
                        Author: ASU.Unbounded_String; 
                        Message: ASU.Unbounded_String) is 
        Traverse: SD.Active_Type_Map.Cursor := SD.Active_Type_Map.First(SD.Active_Clients);
    begin
        while SD.Active_Type_Map.Has_Element (Traverse) = True loop
            if ASU.To_String (SD.Active_Type_Map.Element (Traverse).Key) /= ASU.To_String (Nick_Exception) then       
                LLU.Reset(Buffer.all);
                M.Message_type'Output(Buffer, M.Server);
                ASU.Unbounded_String'Output(Buffer, Author);
                ASU.Unbounded_String'Output(Buffer, Message);
                LLU.Send(SD.Active_Type_Map.Element (Traverse).Value.Client_EP, Buffer);    
            end if;
            SD.Active_Type_Map.Next (Traverse);
        end loop;
    end Send_All;


    procedure Receive_Message   (Buffer: access LLU.Buffer_Type) is
                                 Client_EP: LLU.End_Point_Type;
                                 Nick: ASU.Unbounded_String;
                                 Message: ASU.Unbounded_String;

        Sender_Client: Client;
        Success: Boolean;
    begin
        Client_EP:= LLU.End_Point_Type'Input(Buffer);
        Nick := ASU.Unbounded_String'Input(Buffer);
        Message := ASU.Unbounded_String'Input(Buffer);

        SD.Active_Type_Map.Get(SD.Active_Clients, Nick, Sender_Client, Success);

        if Success = True and then Sender_Client.Client_EP = Client_EP then
            Text_IO.Put_Line("WRITER received from " & ASU.To_String(Nick) & ": " & ASU.To_String(Message));
            Sender_Client.LastMessage := Ada.Calendar.Clock;
            SD.Active_Type_Map.Put(SD.Active_Clients, Nick, Sender_Client);

            Send_All(Buffer, Nick, Nick, Message);
        else
            Text_IO.Put_Line("WRITER received from unknown client. IGNORED");
        end if;


    end Receive_Message;

    
    procedure Client_LogOut (Buffer: access LLU.Buffer_Type) is
        Client_EP: LLU.End_Point_Type;
        Nick: ASU.Unbounded_String;
        Sender_Client: Client;
        Success: Boolean;
    begin
        Client_EP := LLU.End_Point_Type'Input(Buffer);
        Nick := ASU.Unbounded_String'Input(Buffer);
        
        SD.Active_Type_Map.Get(SD.Active_Clients, Nick, Sender_Client, Success);

        if Success = true and then Sender_Client.Client_EP = Client_EP then
            Sender_Client.LastMessage := Ada.Calendar.Clock;
            
            if SD.Inactive_Type_Map.Map_Length(SD.Inactive_Clients) > 149 then
                Text_IO.Put_Line("Inactive Client List is full");
            else
                SD.Inactive_Type_Map.Put(SD.Inactive_Clients, Nick, Sender_Client.LastMessage);
            end if;

            SD.Active_Type_Map.Delete(SD.Active_Clients, Nick, Success);
            Text_IO.Put_Line("LOGOUT received from " & ASU.To_String(Nick));
            Send_All(Buffer, Nick, ASU.To_Unbounded_String("server"), ASU.To_Unbounded_String(ASU.To_String(Nick) & " leaves the chat"));
        else
            Text_IO.Put_Line("LOGOUT received from unknown client. IGNORED");

        end if;
    end Client_LogOut;


    function Time_Image (T: Ada.Calendar.Time) return String is
    begin
        return Gnat.Calendar.Time_IO.Image(T, "%d-%b-%y %T.%i");
    end Time_Image;

    procedure Put_Active_Clients is
        Traverse: SD.Active_Type_Map.Cursor := SD.Active_Type_Map.First(SD.Active_Clients);
        OriginalImage: ASU.Unbounded_String;
        Dir: ASU.Unbounded_String;
        Port: ASU.Unbounded_String;
        StringIndex: Integer;
    begin
        Text_IO.New_Line;
        Text_IO.Put_Line("ACTIVE_CLIENTS");
        Text_IO.Put_Line("===========");
        
    

        while SD.Active_Type_Map.Has_Element (Traverse) = True loop
            OriginalImage:= ASU.To_Unbounded_String(LLU.Image(SD.Active_Type_Map.Element(Traverse).Value.Client_EP));
            Dir := OriginalImage;
            StringIndex:=ASU.Index(Dir, ":");
            Dir := ASU.Tail(Dir, ASU.Length(Dir) - StringIndex);
            StringIndex := ASU.Index(Dir, ",");
            Dir := ASU.Head(Dir, StringIndex - 1);
            StringIndex := ASU.Index(Dir, " ");
            Dir := ASU.Tail(Dir, ASU.Length(Dir) - StringIndex);
            StringIndex := ASU.Index(OriginalImage, ",");
            Port := ASU.Tail(OriginalImage, ASU.Length(OriginalImage) - StringIndex);
            StringIndex := ASU.Index(Port, " ");
            Port := ASU.Tail(Port, ASU.Length(Port) - StringIndex);
            StringIndex := ASU.Index(Port, " ");
            Port := ASU.Tail(Port, ASU.Length(Port) - (StringIndex + 1));
            
            Text_IO.Put_Line(ASU.To_String(SD.Active_Type_Map.Element(Traverse).Key) & 
                            " (" & ASU.To_String(Dir) & ":" & ASU.To_String(Port) & ")" & ": " &
                            Time_Image(SD.Active_Type_Map.Element(Traverse).Value.LastMessage));
            SD.Active_Type_Map.Next(Traverse);
        end loop;

    end Put_Active_Clients;


    procedure Put_Inactive_Clients is
        Traverse: SD.Inactive_Type_Map.Cursor := SD.Inactive_Type_Map.First(SD.Inactive_Clients);
    begin
        Text_IO.New_Line;
        Text_IO.Put_Line("OLD CLIENTS");
        Text_IO.Put_Line("===========");


        while SD.Inactive_Type_Map.Has_Element (Traverse) = True loop
                Text_IO.Put_Line(ASU.To_String(SD.Inactive_Type_Map.Element(Traverse).Key) & ": " &
                                Time_Image(SD.Inactive_Type_Map.Element(Traverse).Value));
                SD.Inactive_Type_Map.Next(Traverse);
            end loop;

    end Put_Inactive_Clients;
end Server_Database;