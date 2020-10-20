with Text_IO;
with Lower_Layer_UDP;
with Ada.Command_Line;
with Ada.Exceptions;
with Handlers;
with Ada.Strings.Unbounded;
with Server_Database;
with Chat_Messages;
with Ada.Calendar;

procedure Chat_Server_2 is
    package LLU renames Lower_Layer_UDP;
    package ASU renames Ada.Strings.Unbounded;
    package SD renames Server_Database;
    package M renames Chat_Messages;
    package ACL renames Ada.Command_Line;

    use type Ada.Calendar.Time;



    Port: Integer; 
    Max: Integer;
    Server_EP: LLU.End_Point_Type;
    Pressed: Character;
begin

    if ACL.Argument_Count /= 2 then
        Text_IO.Put_Line("Usage ./chat_server_2 <port> <max users (2..50)>");
        LLU.Finalize;
        return;
    end if;
    Port := Integer'Value(ACL.Argument(1));
    Max := Integer'Value(ACL.Argument(2));
    if Max < 2 or Max > 50 then
        Text_IO.Put_Line("Usage ./chat_server_2 <port> <max users (2..50)>");
        LLU.Finalize;
        return;
    end if;
    
    Server_EP := LLU.Build(LLU.To_IP(LLU.Get_Host_Name), Port);
    LLU.Bind(Server_EP, Handlers.Server_Receiver'access);
        loop
            Text_IO.Get_Immediate(Pressed);
            if Pressed = 'o' or Pressed = 'O' then
                Server_Database.Put_Inactive_Clients;
            elsif Pressed = 'l' or Pressed = 'L' then
                Server_Database.Put_Active_Clients;
            end if;
        end loop;
    


    exception
        when Ex:others =>
            Text_IO.Put_Line(Ada.Exceptions.Exception_Name(Ex) &
                        ": " & Ada.Exceptions.Exception_Message(Ex));
            LLU.Finalize;
end Chat_Server_2;