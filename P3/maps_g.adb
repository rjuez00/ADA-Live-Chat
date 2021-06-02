--autor: Adrián Lanza
with Text_IO;
with Ada.Unchecked_Deallocation;

package body Maps_G is
	procedure Free is new Ada.Unchecked_Deallocation(Cell, Cell_A);
	
	procedure Get(  M: Map;
                    Key: in  Key_Type;
                    Value: out Value_Type;
                    Success : out Boolean) is

        P_Aux: Cell_A;
    begin
		P_Aux := M.P_First;

		while P_Aux /= null and then P_Aux.Key /= Key loop
		   P_Aux := P_Aux.Next;
		end loop;

		if P_Aux = null then
			Success := False;
		else
			Value := P_Aux.Value;
			Success := True;
		end if;
    end Get;


	procedure Put(  M: in out Map;
		            Key: Key_Type;
		            Value: Value_Type) is
        P_Aux: Cell_A;
		P_Previous: Cell_A;
    begin
		P_Aux := M.P_First;
		while P_Aux /= null and then P_Aux.Key /= Key loop
			P_Previous := P_Aux;
		   	P_Aux := P_Aux.Next;
		end loop;
		
		if P_Aux = null and P_Previous = null then
			M.P_First := new Cell'(Key, Value, null);
			M.Length := M.Length +1;
		elsif P_Aux = null and P_Previous /= null then
			P_Previous.Next :=  new Cell'(Key,Value,null);
			M.Length := M.Length +1;
		else
			P_Aux.Value := Value;
		end if;
    end Put;



	procedure Delete(   M: in out Map;
		                Key: in  Key_Type;
		                Success: out Boolean) is
    	P_Aux: Cell_A;
		P_Previous: Cell_A;
    begin
		P_Aux := M.P_First;
		P_Previous := null;
		while P_Aux /= null and then P_Aux.Key /= Key loop
			P_Previous := P_Aux;
		   	P_Aux := P_Aux.Next;
		end loop;
		Success := True;
		if P_Aux /= null and P_Previous /= null then
			P_Previous.Next := P_Aux.Next;
			Free(P_Aux);
			M.Length := M.Length -1;
		elsif P_Aux /= null and P_Previous = null then
			M.P_First := P_Aux.Next;
			Free(P_Aux);
			M.Length := M.Length -1;
		else
			Success := False;
		end if;

    end Delete;



	function Map_Length(M : Map) return Natural is
    begin
		return M.Length;
    end Map_Length;



	---------------------------
	-- CURSOR IMPLEMENTATION --
	---------------------------

	function First (M: Map) return Cursor is

	begin
		return (M, M.P_First);
	end First;


	procedure Next (C: in out Cursor) is
	begin
		C.Element_A := C.Element_A.Next;
	end Next;


	function Has_Element (C: Cursor) return Boolean is

	begin
		return C.Element_A /= null;
	end Has_Element;


   	-- Raises No_Element if Has_Element(C) = False;
	function Element (C: Cursor) return Element_Type is

	begin
		if Has_Element(C) = False then
			raise No_Element;
		end if;
		return (C.Element_A.Key, C.Element_A.Value);
	end Element;


end Maps_G;