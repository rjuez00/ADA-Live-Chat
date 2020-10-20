-- autor: Adrián Lanza

package body Hash_Maps_G is


    procedure Lazy_Search   (M: in Map; 
                             Key: in Key_Type; 
                             Empty_Index: out Integer;  --empty space where stored (otherwise -1)
                             Found_Index: out Integer; --if found stored index here (otherwise -1)
                             First_Deleted: out Integer) is --first hole with deleted mark (otherwise -1) (always will be before Empty_Hole)

        Original_Index: Integer;
    begin
        Original_Index := Integer(Hash(Key));
        Found_Index := Original_Index;
        First_Deleted := -1;
        Empty_Index := -1;

        while Found_Index < HASH_SIZE loop
            if M.Containers(Found_Index).Content = Empty then
                Empty_Index := Found_Index;
                Found_Index := -1;
                exit;
            elsif M.Containers(Found_Index).Content = Deleted and First_Deleted = -1 then
                First_Deleted := Found_Index;
            elsif M.Containers(Found_Index).Content = Full and M.Containers(Found_Index).Key = Key then
                exit;
            end if;

            Found_Index := (Found_Index +1) mod HASH_SIZE;
            if Found_Index = Original_Index then
                Found_Index := -1;
                exit;
            end if;
        end loop;
    end Lazy_Search;


    procedure Get   (M : in out Map;
                    Key : in Key_Type;
                    Value : out Value_Type;
                    Success : out Boolean) is
        Empty_Index: Integer;
        Found_Index: Integer;
        First_Deleted: Integer;
    begin
        Lazy_Search(M, Key, Empty_Index, Found_Index, First_Deleted);
        
        if Found_Index /= -1 then
            Value := M.Containers(Found_Index).Value;
            Success := True;
            if First_Deleted /= -1 then --move to the first index
                M.Containers(First_Deleted).Key := M.Containers(Found_Index).Key;
                M.Containers(First_Deleted).Value := M.Containers(Found_Index).Value;
                M.Containers(First_Deleted).Content := Full;
                M.Containers(Found_Index).Content := Deleted;
            end if;
        end if;
    end Get;
    

    procedure Put (M : in out Map;
                    Key : Key_Type;
                    Value : Value_Type) is

        Empty_Index: Integer;
        Found_Index: Integer;
        First_Deleted: Integer;

        Final_Destination: Integer;
    begin
        Lazy_Search(M, Key, Empty_Index, Found_Index, First_Deleted);
        if Found_Index = -1 then
            if M.Length >= Max then 
                raise Full_Map;
            end if;

            Final_Destination := Empty_Index;
            if First_Deleted /= -1 then
                Final_Destination := First_Deleted;
            end if;
            M.Length := M.Length +1;
            M.Containers(Final_Destination).Key := Key;
            M.Containers(Final_Destination).Value := Value;
            M.Containers(Final_Destination).Content := Full;
        end if;
    end Put;


    procedure Delete    (M : in out Map;
                        Key : in Key_Type;
                        Success : out Boolean) is
        Empty_Index: Integer;
        Found_Index: Integer;
        First_Deleted: Integer;
    begin
        Success := False;
        Lazy_Search(M, Key, Empty_Index, Found_Index, First_Deleted);
        if Found_Index /= -1 then
            Success := True;
            M.Containers(Found_Index).Content := Deleted;
            M.Length := M.Length -1;
        end if;
    end Delete;


    function Map_Length (M : Map) return Integer is
    
    begin
        return M.Length;
    end Map_Length;

    function First (M: Map) return Cursor is
        C:Cursor;
        Position: Integer := 0;
    begin
        C.Position := -1;
        while Position < HASH_SIZE loop
			if M.Containers(Position).Content = Full then
                C.Position := Position;
				exit;
			end if;
            
            Position := (Position +1) mod HASH_SIZE;
            if Position = 0 then
                Position := -1;
                exit;
            end if;
		end loop;
        C.M:=M;
        return C;
    end First;

    procedure Next (C: in out Cursor) is
	begin
        if Has_Element(C) then
			if C.Position = HASH_SIZE - 1 then
				C.Position := -1;
			else

                C.Position := (C.Position +1) mod HASH_SIZE;
				while C.Position < HASH_SIZE loop
					exit when C.M.Containers(C.Position).Content = Full;


                    C.Position := (C.Position +1) mod HASH_SIZE;
					if C.Position = 0 then
						C.Position := -1;
                        exit;
					end if;
				end loop;
			end if;
		end if;
	end Next;




    function Has_Element (C: Cursor) return Boolean is
    begin
        return C.Position /= -1;
    end Has_Element;

    -- Raises No_Element if Has_Element(C) = False;
    function Element (C: Cursor) return Element_Type is
    begin
        if Has_Element(C) = False then
            raise No_Element;
        end if;
        return (C.M.Containers(C.Position).Key, C.M.Containers(C.Position).Value);
    end Element;
    
end Hash_Maps_G;