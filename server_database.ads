with Hash_Maps_G;
with Ordered_Maps_G;
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Lower_Layer_UDP;

package Server_Database is
    package LLU renames Lower_Layer_UDP;
    package ASU renames Ada.Strings.Unbounded;

    type Client is record
        Client_EP : LLU.End_Point_Type;
        LastMessage : Ada.Calendar.Time;
    end record;

    HASH_SIZE:   constant := 150;
    type Hash_Range is mod HASH_SIZE;
    function ASU_Hash (ToHash: ASU.Unbounded_String) return Hash_Range;


    package Active_Type_Map is new Hash_Maps_G (Key_Type   => ASU.Unbounded_String,
                                                Value_Type => Client,
                                                "="        => ASU."=",
                                                Hash_Range => Hash_Range,
                                                Hash       => ASU_Hash,
                                                Max        => 150);

    package Inactive_Type_Map is new Ordered_Maps_G (Key_Type => ASU.Unbounded_String, 
                                                    Value_Type => Ada.Calendar.Time, 
                                                    "=" => ASU."=",
                                                    "<" => ASU."<",
                                                    Max => 150);

    Active_Clients: Active_Type_Map.Map;
    Inactive_Clients: Inactive_Type_Map.Map;

    procedure Client_LogOut (Buffer: access LLU.Buffer_Type);
    procedure Receive_Message (Buffer: access LLU.Buffer_Type);
    procedure Register (Buffer: access LLU.Buffer_Type);
    procedure Expell_Oldest (Buffer: access LLU.Buffer_Type);
    procedure Send_All (Buffer: access LLU.Buffer_Type; 
                        Nick_Exception: ASU.Unbounded_String; 
                        Author: ASU.Unbounded_String; 
                        Message: ASU.Unbounded_String);

    procedure Put_Active_Clients;
    procedure Put_Inactive_Clients;


    
end Server_Database;
