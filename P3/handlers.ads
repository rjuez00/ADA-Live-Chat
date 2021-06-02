-- autor: Adrián Lanza

with Lower_Layer_UDP;


package Handlers is
    package LLU renames Lower_Layer_UDP;
    
    procedure Client_Receiver ( From: in LLU.End_Point_Type;
                                To: in LLU.End_Point_Type;
                                Buffer: access LLU.Buffer_Type);

    procedure Server_Receiver   (From: in  LLU.End_Point_Type;
                                 To: in LLU.End_Point_Type;
                                 Buffer: access LLU.Buffer_Type);
end Handlers;