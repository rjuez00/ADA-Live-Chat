package body Ordered_Maps_G is 
    procedure B_Search  (M: in Map;
                         Key: in Key_Type;
                         Index: out Integer;
                         Success: out Boolean) is
        L: Integer;
        R: Integer;
        Mid: Integer;
    begin

        Success := False;
        L := 1; -- deberia ser 0 pero calcular la mitad sale el indice con indices de 1 a la longitud
        R := M.Length;

        while L <= R loop
            Mid := (R+L)/2; 
            if M.Containers(Mid-1).Key = Key then --resto -1 para que cuadre con los indices 0..Max
                Success := True;
                Index := Mid-1;
                exit;
            else 
                if Key < M.Containers(Mid-1).Key then R := Mid-1;
                else L := Mid+1; end if;
            end if;
        end loop;
        
        Mid := Mid-1;
        if Success = False and M.Length > 0 then --must correct index (sometimes shifted by 1)
            if Key < M.Containers(Mid).Key then --happens if the target key is larger than the target spot
                Index := Mid;                     -- it ends up in the lower spot, so +1
            else 
                Index := Mid+1;
            end if;
        elsif Success = False and M.Length = 0 then
            Index := 0;
        end if;
    end B_Search;


    procedure Get   (M : Map;
                     Key : in Key_Type;
                     Value : out Value_Type;
                     Success : out Boolean) is
        Index: Integer;
    begin
        B_Search(M, Key, Index, Success);
        if Success = True then
            Value := M.Containers(Index).Value;
        end if;
    end Get;

    procedure Put   (M : in out Map;
                     Key : Key_Type;
                     Value : Value_Type) is
        Index: Integer;
        Success: Boolean;
    begin
        B_search(M, Key, Index, Success);
        if Success = True then
            M.Containers(Index).Value := Value;
            return;
        end if;

        if Max <= M.Length then
            raise Full_Map;
        end if;

        for loop_i in reverse Index..M.Length-1 loop
			M.Containers(loop_i + 1) := M.Containers(loop_i);
		end loop;
        M.Containers(Index).Key := Key;
        M.Containers(Index).Value := Value;
        M.Length := M.Length +1;


    end Put;

    procedure Delete (M : in out Map;
                      Key : in Key_Type;
                      Success : out Boolean) is
        Index: Integer;
    begin
        B_Search(M, Key, Index, Success);
        if Success = False then 
            return;
        end if;
        for loop_i in Index..M.Length-2 loop -- -2 = -1 por los indices desde 0 y -1 por mover el penultimo al ultimo, el ultimo no lo mueves a ningun sitio pues estaria lleno
			M.Containers(loop_i) := M.Containers(loop_i+1);
		end loop;
        M.Length := M.Length -1;
    end Delete;


    function Map_Length (M : Map) return Natural is
    begin
        return M.Length;
    end Map_Length;
    


    function First (M: Map) return Cursor is
        C: Cursor;
    begin
        if Map_Length(M) > 0 then
            C.Position := 0;
        else 
            C.Position := -1;
        end if;
        C.M := M;
        
        return C;
    end First;
    
    procedure Next (C: in out Cursor) is
    begin
        if Has_Element(C) = False then 
            return;
        end if;

        if C.Position >= C.M.Length-1 then
            C.Position := -1;
        else
            C.Position := C.Position +1;
        end if;
    end Next;
    
    
    function Has_Element (C: Cursor) return Boolean is
    begin
        return C.Position /= -1;
    end Has_element;


    -- Raises No_Element if Has_Element(C) = False;
    function Element (C: Cursor) return Element_Type is
    begin
        if Has_element(C) = False then
            raise No_Element;
        end if;
        return (C.M.Containers(C.Position).Key, C.M.Containers(C.Position).Value);
    end Element;

end Ordered_Maps_G;