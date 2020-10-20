-- autor: Adrián Lanza

generic
    type Key_Type is private;
    type Value_Type is private;
    with function "=" (K1, K2: Key_Type) return Boolean;
    type Hash_Range is mod <>;
    with function Hash (K: Key_Type) return Hash_Range;
    Max: in Natural;

package Hash_Maps_G is
    type Map is limited private;
    Full_Map : exception;

    procedure Get   (M : in out Map;
                    Key : in Key_Type;
                    Value : out Value_Type;
                    Success : out Boolean);

    procedure Put   (M : in out Map;
                    Key : Key_Type;
                    Value : Value_Type);

    procedure Delete    (M : in out Map;
                        Key : in Key_Type;
                        Success : out Boolean);

    function Map_Length (M : Map) return Integer;

    --
    -- Cursor Interface for iterating over Map elements
    --
    type Cursor is limited private;
    function First (M: Map) return Cursor;
    procedure Next (C: in out Cursor);
    function Has_Element (C: Cursor) return Boolean;

    type Element_Type is record
        Key: Key_Type;
        Value: Value_Type;
    end record;

    No_Element: exception;
    -- Raises No_Element if Has_Element(C) = False;
    function Element (C: Cursor) return Element_Type;

private
    type State is (Full, Empty, Deleted);

    type Container is record
        Key: Key_Type;
        Value: Value_Type;
        Content: State := Empty;
    end record;

    HASH_SIZE: constant Integer := Integer'Max(1 + Integer(Hash_Range'Last - Hash_Range'First), Max);
    type Container_Array is array (0..HASH_SIZE -1) of Container;
    type Container_Array_A is access Container_Array;
    type Map is record
        Containers: Container_Array_A := new Container_Array;
        Length: Integer := 0;
    end record;

    type Cursor is record
        M: Map;
        Position: Integer;
    end record;

end Hash_Maps_G;