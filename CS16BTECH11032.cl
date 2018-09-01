(*
   The class A2I provides integer-to-string and string-to-integer
conversion routines.  To use these routines, either inherit them
in the class where needed, have a dummy variable bound to
something of type A2I, or simple write (new A2I).method(argument).
*)


(*
   c2i   Converts a 1-character string to an integer.  Aborts
         if the string is not "0" through "9"
*)
class A2I {

     c2i(char : String) : Int {
        if char = "0" then 0 else
        if char = "1" then 1 else
        if char = "2" then 2 else
        if char = "3" then 3 else
        if char = "4" then 4 else
        if char = "5" then 5 else
        if char = "6" then 6 else
        if char = "7" then 7 else
        if char = "8" then 8 else
        if char = "9" then 9 else
        { abort(); 0; }  (* the 0 is needed to satisfy the typechecker *)
        fi fi fi fi fi fi fi fi fi fi
     };

(*
   i2c is the inverse of c2i.
*)
     i2c(i : Int) : String {
        if i = 0 then "0" else
        if i = 1 then "1" else
        if i = 2 then "2" else
        if i = 3 then "3" else
        if i = 4 then "4" else
        if i = 5 then "5" else
        if i = 6 then "6" else
        if i = 7 then "7" else
        if i = 8 then "8" else
        if i = 9 then "9" else
        { abort(); ""; }  -- the "" is needed to satisfy the typchecker
        fi fi fi fi fi fi fi fi fi fi
     };

(*
   a2i converts an ASCII string into an integer.  The empty string
is converted to 0.  Signed and unsigned strings are handled.  The
method aborts if the string does not represent an integer.  Very
long strings of digits produce strange answers because of arithmetic
overflow.

*)
     a2i(s : String) : Int {
        if s.length() = 0 then 0 else
        if s.substr(0,1) = "-" then ~a2i_aux(s.substr(1,s.length()-1)) else
        if s.substr(0,1) = "+" then a2i_aux(s.substr(1,s.length()-1)) else
           a2i_aux(s)
        fi fi fi
     };

(* a2i_aux converts the usigned portion of the string.  As a
   programming example, this method is written iteratively.  *)


     a2i_aux(s : String) : Int {
        (let int : Int <- 0 in
           {
               (let j : Int <- s.length() in
                  (let i : Int <- 0 in
                    while i < j loop
                        {
                            int <- int * 10 + c2i(s.substr(i,1));
                            i <- i + 1;
                        }
                    pool
                  )
               );
              int;
            }
        )
     };

(* i2a converts an integer to a string.  Positive and negative
   numbers are handled correctly.  *)

    i2a(i : Int) : String {
        if i = 0 then "0" else
        if 0 < i then i2a_aux(i) else
          "-".concat(i2a_aux(i * ~1))
        fi fi
    };

(* i2a_aux is an example using recursion.  *)

    i2a_aux(i : Int) : String {
        if i = 0 then "" else
            (let next : Int <- i / 10 in
                i2a_aux(next).concat(i2c(i - next * 10))
            )
        fi
    };

};

-- This class is used to represent individual element of the stack
class StackElem inherits IO {
    -- Member variable of class which is the value of that stack element
  	val : String;
    -- Member variable of class which is like a pointer to the next object
    nxt : StackElem;

    -- init() function intialises the value of the object
    init(x : String) : Object {
        val <- x
    };

    -- addLink() function adds the next object to this object
    addLink(x : StackElem) : Object {
        nxt <- x
    };

    -- elemValue() returns the value of the stack element
    elemValue() : String {
        val
    };

    -- nxtValue() returns the next object of the stack
    nxtValue() : StackElem {
        nxt
    };
};

-- Main class manages the I/O and calling the required functions
class Main inherits IO {
    inStr : String;
    headPointer : StackElem;
    noOfElem : Int;
    atoiObj : A2I;
    result : String;
    topElem : StackElem;
    top : String;
    loopVar : Bool;
    val1 : String;
    val2 : String;
    tmpElem : StackElem;
    retStr : String;

    -- main() takes the input from the user and does the operations as defined
    main() : Object {{
        noOfElem <- 1;
        atoiObj <- new A2I;
        loopVar <- true;

        -- We take input in a loop and terminate when user inputs 'x' symbol
        while loopVar loop {
            out_string(">");
            inStr <- in_string();

            if inStr = "x" then {
                abort();
            } else if inStr = "e" then {
                evaluateExp();
            } else if inStr = "d" then {
                printElems();
            } else {
                push(inStr);
                noOfElem <- noOfElem + 1;
            } fi fi fi;

        } pool;
    }};
 
    -- printElems() function prints the current stack contents
    printElems() : Object {{
        topElem <- headPointer;

        -- We print the stack content till the topElem becones 'isvoid'
        while not isvoid topElem loop {
            out_string(topElem.elemValue());
            out_string("\n");
            topElem <- topElem.nxtValue();
        } pool;
    }};

    -- evaluateExp() evaluates the expression on receiving an input 'e'
    evaluateExp() : Object {{
        top <- pop();
        noOfElem <- noOfElem - 1;


        if top = "+" then
            evaluatePlus()
        else if top = "s" then
            evalS()
        else {
            push(top);
            noOfElem <- noOfElem + 1;
        } fi fi;
    }};

    -- If the symbol popped out of stack is '+', then two elements are popped and their sum is pushed back
    evaluatePlus() : Object {{
        val1 <- pop();
        val2 <- pop();
        noOfElem <- noOfElem - 2;
        result <- atoiObj.i2a(atoiObj.a2i(val1) + atoiObj.a2i(val2));
        push(result);
        noOfElem <- noOfElem + 1;
    }};

    -- If the symbol popped is 's', then two elements are popped and they are pushed in after swapping
    evalS() : Object {{
        val1 <- pop();
        val2 <- pop();
        noOfElem <- noOfElem - 2;
        push(val1);
        noOfElem <- noOfElem + 1;
        push(val2);
        noOfElem <- noOfElem + 1;
    }};

    -- Utility function for pushing the element in stack
    push(s : String) : Object {{
        tmpElem <- new StackElem;
        tmpElem.init(s);

        if noOfElem = 1 then
            headPointer <- tmpElem
        else {
            tmpElem.addLink(headPointer);
            headPointer <- tmpElem;
        } fi;
    }};

    -- Utility function for popping the top of the stack
    pop() : String {{
        retStr <- headPointer.elemValue();
        headPointer <- headPointer.nxtValue();
        retStr;
    }};
};
