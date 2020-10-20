--autor: Adrián Lanza

with Ada.Text_IO;
With Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;
with Hash_Maps_G;
with Ada.Exceptions;


procedure Hash_Maps_Test_String is
   package ASU  renames Ada.Strings.Unbounded;
   package ATIO renames Ada.Text_IO;

   HASH_SIZE:   constant := 10;

   type Hash_Range is mod HASH_SIZE;

   function ASU_Hash (ToHash: ASU.Unbounded_String) return Hash_Range is
      Hash:Natural;
   begin
      Hash := 0;
      for I in 1..ASU.Length(ToHash) loop
         Hash := Hash + Character'Pos(ASU.Element(ToHash , I));
      end loop;
      return Hash_Range'Mod(Hash);
   end ASU_Hash;


   package Maps is new Hash_Maps_G (Key_Type   => ASU.Unbounded_String,
                                    Value_Type => Natural,
                                    "="        => ASU."=",
                                    Hash_Range => Hash_Range,
                                    Hash       => ASU_Hash,
                                    Max        => 7);


   procedure Print_Map (M : Maps.Map) is
      C: Maps.Cursor := Maps.First(M);
   begin
      Ada.Text_IO.Put_Line ("Map");
      Ada.Text_IO.Put_Line ("===");

      while Maps.Has_Element(C) loop
         Ada.Text_IO.Put_Line (ASU.To_String(Maps.Element(C).Key) & " " &
                               Natural'Image(Maps.Element(C).Value));
         Maps.Next(C);
      end loop;
   end Print_Map;




   procedure Do_Put (M: in out Maps.Map; K: ASU.Unbounded_String; V: Natural) is
   begin
      Ada.Text_IO.New_Line;
      ATIO.Put_Line("Putting " & ASU.To_String(K));
      Maps.Put (M, K, V);
      Print_Map(M);
   exception
      when Maps.Full_Map =>
         Ada.Text_IO.Put_Line("Full_Map");
   end Do_Put;


   procedure Do_Get (M: in out Maps.Map; K: ASU.Unbounded_String) is
      V: Natural;
      Success: Boolean;
   begin
      Ada.Text_IO.New_Line;
      ATIO.Put_Line("Getting " & ASU.To_String(K));
      Maps.Get (M, K, V, Success);
      if Success then
         Ada.Text_IO.Put_Line("Value:" & Natural'Image(V));
         Print_Map(M);
      else
         Ada.Text_IO.Put_Line("Element not found!");
      end if;
   end Do_Get;


   procedure Do_Delete (M: in out Maps.Map; K: ASU.Unbounded_String) is
      Success: Boolean;
   begin
      Ada.Text_IO.New_Line;
      ATIO.Put_Line("Deleting " & ASU.To_String(K));
      Maps.Delete (M, K, Success);
      if Success then
         Print_Map(M);
      else
         Ada.Text_IO.Put_Line("Element not found!");
      end if;
   end Do_Delete;


   A_Map : Maps.Map;

begin
   Do_Put (A_Map, ASU.To_Unbounded_String("Joi"), 13);
   Do_Put (A_Map, ASU.To_Unbounded_String("Deckard"), 10);
   Do_Put (A_Map, ASU.To_Unbounded_String("Tyrell"), 20);
   Do_Put (A_Map, ASU.To_Unbounded_String("Wallace"), 11);
   Do_Put (A_Map, ASU.To_Unbounded_String("K/Joe"), 15);
   Do_Put (A_Map, ASU.To_Unbounded_String("Rachel"), 16);
   
   Do_Delete (A_Map, ASU.To_Unbounded_String("K/Joe"));
   Do_Delete (A_Map, ASU.To_Unbounded_String("Tyrell"));
   Do_Delete (A_Map, ASU.To_Unbounded_String("Luke"));  -- Does not exist
   
   Do_Delete (A_Map, ASU.To_Unbounded_String("DOESNT EXIST"));  -- Does not exist

   Do_Put (A_Map, ASU.To_Unbounded_String("Elliot"), 25);
   Do_Put (A_Map, ASU.To_Unbounded_String("Mr. Robot"), 75);
   Do_Put (A_Map, ASU.To_Unbounded_String("Darlene"), 73);

   Do_Delete (A_Map, ASU.To_Unbounded_String("Darlene"));
   Do_Get (A_Map, ASU.To_Unbounded_String("Mr. Robot"));
   Do_Put (A_Map, ASU.To_Unbounded_String("Dom"), 22);
   Do_Delete (A_Map, ASU.To_Unbounded_String("Dom"));
   Do_Get (A_Map, ASU.To_Unbounded_String("Mr. Robot"));
   Do_Put (A_Map, ASU.To_Unbounded_String("Philip Price"), 30);
   Do_Put (A_Map, ASU.To_Unbounded_String("WhiteRose"), 40);
   Do_Put (A_Map, ASU.To_Unbounded_String("SHOULD REPORT FULL_MAP"), 40);

   Do_Get (A_Map, ASU.To_Unbounded_String("NO EXISTE"));     -- Does not exist



   exception
        when Ex:others =>
            ATIO.Put_Line(Ada.Exceptions.Exception_Name(Ex) &
                        ": " & Ada.Exceptions.Exception_Message(Ex));
end Hash_Maps_Test_String;
