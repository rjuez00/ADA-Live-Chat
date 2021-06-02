with Ada.Unchecked_Deallocation;

package body Hash_Maps_G is
	procedure Free is new Ada.Unchecked_Deallocation(Linked_List, Linked_List_A);


    procedure Get   (M : in out Map;
                    Key : in Key_Type;
                    Value : out Value_Type;
                    Success : out Boolean) is
        Iterator: Linked_List_A;
        Found: Boolean := False;
    begin
        Iterator := M.Containers(Hash(Key));
        Success := False;
        while Iterator /= null and then Found = False loop
            if Iterator.Key = Key then
                Found := True;
                Success := True;
                Value := Iterator.Value;
            end if; 
            Iterator := Iterator.Next;
        end loop;
    end Get;
    

    procedure Put (M : in out Map;
                    Key : Key_Type;
                    Value : Value_Type) is

        Iterator: Linked_List_A;
        Previous: Linked_List_A;
    begin
        Iterator := M.Containers(Hash(Key));
        if Iterator = null and then M.Length < Max then
            M.Containers(Hash(key)) := new Linked_List'(Key, Value, null);
            M.Length := M.Length +1;
        elsif Iterator /= null then
            while Iterator /= null and then Iterator.Key /= Key loop
               Previous := Iterator;
               Iterator := Iterator.Next;
            end loop;

            if Iterator /= null then
                Iterator.Value := Value;
            elsif M.Length < Max then
                Previous.Next := new Linked_List'(Key,Value, null);
                M.Length := M.Length +1;
            else 
                raise Full_Map;
            end if;
        else 
            raise Full_Map;
        end if;
    end Put;


    procedure Delete    (M : in out Map;
                        Key : in Key_Type;
                        Success : out Boolean) is
        Iterator: Linked_List_A;
        Previous: Linked_List_A := null;
    begin
        Iterator := M.Containers(Hash(Key));
        while Iterator /= null and then Iterator.Key /= Key loop
               Previous := Iterator;
               Iterator := Iterator.Next;
        end loop;

        if Iterator /= null and Previous /= null then --is in the middle of list
            Previous.Next := Iterator.Next;
            Free(Iterator);
            Success := True;
            M.Length := M.Length -1;
        elsif Iterator /= null and Previous = null then --first one in list
            M.Containers(Hash(Key)) := Iterator.Next;
            Free(Iterator);
            Success := True;
            M.Length := M.Length -1;
        else 
            Success := False;
        end if;        
    end Delete;


    function Map_Length (M : Map) return Natural is
    
    begin
        return M.Length;
    end Map_Length;

    function First (M: Map) return Cursor is
        Index: Hash_Range;
        Found: Boolean := False;
        ReturnCursor: Cursor;
    begin
        Index := 0;
        if M.Length > 0 then
            while Found = False loop
                if M.Containers(Index) = null then
                    Index := Index +1;
                else 
                    Found := True;
                    ReturnCursor := (M, Index, M.Containers(Index));
                end if;
            end loop;
        else
            ReturnCursor := (M, 0, null);
        end if;
        return ReturnCursor;
    end First;

    procedure Next (C: in out Cursor) is
        Found: Boolean;
		I: Hash_Range;
	begin
		if C.Element /= null then
			C.Element := C.Element.Next;
			if C.Element = null then
				Found := False;
				I := C.Position + 1;
				if I /= Hash_Range'First then
					loop
						if C.Hash.Containers(I) /= null then
							Found := True;
							C.Position := I;
							C.Element := C.Hash.Containers(I);
						end if;
						I := I + 1;
					exit when Found or I = Hash_Range'First;
					end loop;
				end if;
            
			end if;

		end if;
	end Next;

    function Has_Element (C: Cursor) return Boolean is

    begin
        return C.Element /= null;
    end Has_Element;

    -- Raises No_Element if Has_Element(C) = False;
    function Element (C: Cursor) return Element_Type is

    begin
        if C.Element = null then
            raise No_Element;
        else
            return (C.Element.Key, C.Element.Value);
        end if;
    end Element;
    
end Hash_Maps_G;