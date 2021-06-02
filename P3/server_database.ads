-- autor: Adrián Lanza

with Maps_G;
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

    package Active_Type_Map is new Maps_G    (ASU.Unbounded_String,
                                             Client,
                                             ASU."=");
    package Inactive_Type_Map is new Maps_G  (ASU.Unbounded_String,
                                             Ada.Calendar.Time,
                                             ASU."=");

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
