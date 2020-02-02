//
// Parser for simple C programs.  This component checks 
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a tuple containing 
// 3 values:  
//
//   (result, msg, program)
//
// where result is true or false (legal or not legal), 
// msg is a success or syntax error message, and program
// is a list of instructions if parsing was successful.
//
// <<Hamza Amjad>>
// U. of Illinois, Chicago
//

#light

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // These are debug routines that output the tokens, or
  // program, respectively.  They are written so you can
  // inject these into a pipeline to output the current 
  // state of the tokens or program.
  //
  let private __outputTokens (tokens, program) =
    printfn "Tokens: %A" tokens
    (tokens, program)

  let private __outputProgram (tokens, program) =
    printfn "Program: %A" program
    (tokens, program)


  //
  // matchToken
  //
  let private matchToken expected_token (tokens, program) =
    let (token, _) = List.head tokens
    //
    // if the token matches the expected token, keep parsing by
    // returning the rest of the tokens.  Otherwise throw an
    // exception because there's a syntax error, effectively 
    // stopping compilation:
    //
    if expected_token = token then  
      (List.tail tokens, program)
    else
      failwith ("expecting " + (string expected_token) + ", but found " + (string token))

   // *************************************************************************************
   // -----------------Stmts function-----------------------
  let rec private stmts (tokens, program) = 
    let (t, p) = stmt ( tokens, program)
    let (t1, p1) = morestmts (t, p)
    (t1, p1)
  // -----------------morestmts function---------------------   
  and private morestmts (tokens, program) = 
    let (nextToken, _) = List.head tokens
    if (nextToken = lexer.Tokens.CloseBrace) then
        (tokens, program)
    else
        let (T, P) = stmt (tokens, program)
        let (T1, P1) = morestmts (T, P)
        (T1, P1)

    // ------------------stmt------------------------------ 
  and private stmt (tokens, program) = 
    let (nextToken, _) = List.head tokens
    if (nextToken = lexer.Tokens.Semicolon) then
        let (t,p) = empty_part (tokens, program)
        (t, p)
    else if (nextToken = lexer.Tokens.Int) then
        let (t1, p1) = vardecl(tokens, program)
        (t1, p1)
    else if (nextToken = lexer.Tokens.Cin) then
        let (t2, p2) = input_part (tokens, program)
        (t2, p2)
    else if (nextToken = lexer.Tokens.Cout) then
        let (t3, p3) = output_part (tokens, program)
        (t3, p3)
    else if (nextToken = lexer.Tokens.ID) then
        let (t4, p4) = assignment_part (tokens, program)
        (t4, p4)
    else if (nextToken = lexer.Tokens.If) then
        let (t5, p5) = if_part (tokens, program)
        (t5, p5)
    else
        failwith ("expecting statement, but found " + (string nextToken))

 //--------------------------then_part-------------------------------------
  and private then_part (tokens, program) =
    let (t, p) = stmt(tokens, program)
    (t, p)
    //----------------------else_part---------------------------------------
  and private else_part (tokens, program) = 
    let (nextToken, _) = List.head tokens

    if (nextToken = lexer.Tokens.Else) then
        let (T1, P1) = matchToken lexer.Tokens.Else (tokens, program)
        let (T2, P2) = stmt ( T1, P1)
        (T2, P2)
    else
        (tokens, ["$EMPTY"]::program)
  
  //------------------------ifstmt----------------------------------------
  and private if_part ( tokens, program) = 
    let (nextToken, _) = List.head tokens

    if (nextToken = lexer.Tokens.If) then
        let (t, p) = matchToken lexer.Tokens.If (tokens, program)
        let (t1, p1) = matchToken lexer.Tokens.OpenParen (t, p)
        let (t2, p2) = condition_part ( t1, p1)
        let (t3, p3) = matchToken lexer.Tokens.CloseParen (t2, p2)
        let (t4, p4) = then_part ( t3, p3)
        let (t5, p5) = else_part (t4, p4)

        (t5, p5)

        
    else
        failwith ("Expecting IF, but found " + (string nextToken))

   // -----------------------empty-------------------------------
  and private empty_part (tokens, program) =
    let (nextToken, _) = List.head tokens
    //
    if nextToken = lexer.Tokens.Semicolon then
        let (t1, p1) = matchToken lexer.Tokens.Semicolon (tokens, program)
        (t1, ["$EMPTY"]::p1)
    else
        failwith ("Expecting semicolon, but found "  + (string tokens))

  
    
  // ----------------------vardecl-------------------------------
  and private vardecl (tokens, program) = 
    let (nextToken, _) = List.head tokens
    if nextToken = lexer.Tokens.Int then
        let (t , p) = matchToken lexer.Tokens.Int (tokens, program) 
        let (t2 , p2) = matchToken lexer.Tokens.ID (t, p)
         
        let (t3, p3) = matchToken lexer.Tokens.Semicolon (t2, p2)
        
        let (_, vale) = List.head t
  
        (t3,["$DECL"; vale]::p3)
    else
        failwith ("expecting int or id or semicolon, but found " + string nextToken)
 
 // ----------------------------input----------------------------------
  and private input_part (tokens, program) =
    let (nextToken, _) = List.head tokens
    if nextToken = lexer.Tokens.Cin then
        let (t,p) = matchToken lexer.Tokens.Cin (tokens, program)
        let (t1, p1) = matchToken lexer.Tokens.Input (t, p)
        let (t2, p2) = matchToken lexer.Tokens.ID (t1, p1)
        let (t3, p3) = matchToken lexer.Tokens.Semicolon (t2, p2)

        let (str, vale) = List.head t1
       

        (t3, ["$INPUT"; vale]::p3)
    else
        failwith ("expecting cin or >> or id or semicolon, but found " + string nextToken)
  

  //------------------------------expr_op----------------------------------
  and private expr_op (tokens, program) = 
    let (nextToken, _) = List.head tokens
    if (nextToken = lexer.Tokens.Plus) then
        let (t, p) = matchToken lexer.Tokens.Plus (tokens, program)
        (t,p)
    else if (nextToken = lexer.Tokens.Minus) then
        let (t1, p1) = matchToken lexer.Tokens.Minus (tokens, program)
        (t1, p1)
    else if (nextToken = lexer.Tokens.Times) then
        let (t2, p2) = matchToken lexer.Tokens.Times (tokens, program)
        (t2, p2)
    else if (nextToken = lexer.Tokens.Divide) then
        let (t3, p3) = matchToken lexer.Tokens.Divide (tokens, program)
        (t3, p3)
    else if (nextToken = lexer.Tokens.Power) then
        let (t4, p4) = matchToken lexer.Tokens.Power (tokens, program)
        (t4, p4)
    else if (nextToken = lexer.Tokens.LT) then
        let (t5, p5) = matchToken lexer.Tokens.LT (tokens, program)
        (t5, p5)
    else if ( nextToken = lexer.Tokens.LTE) then
        let ( t6, p6) = matchToken lexer.Tokens.LTE (tokens, program)
        (t6, p6)
    else if (nextToken = lexer.Tokens.GT) then
        let (t7, p7) = matchToken lexer.Tokens.GT (tokens, program)
        (t7 , p7)
    else if (nextToken = lexer.Tokens.GTE) then
        let (t8 , p8) = matchToken lexer.Tokens.GTE (tokens, program)
        (t8, p8)
    else if (nextToken = lexer.Tokens.EQ) then
        let (t9, p9) = matchToken lexer.Tokens.EQ (tokens, program)
        (t9, p9)
    else if (nextToken = lexer.Tokens.NE) then
        let (t10, p10) = matchToken lexer.Tokens.NE (tokens, program)
        (t10, p10)
    else
        failwith ("expecting +,-,/,^, but found " + string nextToken)
  
  
  // ---------------------------expr_value--------------------------------------
  and private expr_value (tokens, program) =
    let (nextToken, _) = List.head tokens
    if (nextToken = lexer.Tokens.ID) then
        let (t, p) = matchToken lexer.Tokens.ID (tokens, program)
        (t, p)
    else if (nextToken = lexer.Tokens.Int_Literal) then
        let (t1, p1) = matchToken lexer.Tokens.Int_Literal (tokens, program)
        (t1, p1)
    else if (nextToken = lexer.Tokens.Str_Literal) then
        let (t2, p2) = matchToken lexer.Tokens.Str_Literal (tokens, program)
        (t2, p2)
    else if (nextToken = lexer.Tokens.Bool_Literal) then
        let (t3, p3) = matchToken lexer.Tokens.Bool_Literal (tokens, program)
        (t3, p3)
    else
        failwith ("expecting id or int or str or bool, but found " + (string nextToken))
   




   // ---------------------------------expr-------------------------------
   // handling the following conditions
   //   individuals--> ID/Int/Str/Bool
   //   equations--> ID (op) Int or Int (op) ID
   //   Error checking--> ID (op) "Str"/Bool is ilegal
   //   Bool--> false - true and so on
   //
  and private expr_part (tokens, program) = 
    let (nextToken, _) = List.head tokens
    
    if (nextToken = lexer.Tokens.ID) || (nextToken = lexer.Tokens.Int_Literal) then
        let (t1, p1) = expr_value (tokens, program)
        let (tok, _) = List.head t1
        if (tok = lexer.Tokens.Minus) || (tok = lexer.Tokens.Plus) || (tok = lexer.Tokens.Times) || (tok = lexer.Tokens.Power) || (tok = lexer.Tokens.Divide) 
            || (tok = lexer.Tokens.LT) || (tok = lexer.Tokens.LTE) || (tok = lexer.Tokens.GT) || (tok = lexer.Tokens.GTE) || (tok = lexer.Tokens.EQ) || (tok = lexer.Tokens.NE) then
            let (t2, p2) = expr_op (t1, p1)
            let (nextTok, _) = List.head t2
            if (tok = lexer.Tokens.EQ) || (tok = lexer.Tokens.NE) then
                 if (nextTok = lexer.Tokens.Str_Literal) then
                    let (t3, p3) = expr_value (t2, p2)
                    (t3, p3)
                    
                 else   
                    let (t3, p3) = expr_value (t2, p2)
                    (t3, p3)
            else if (tok <> lexer.Tokens.EQ) || (tok <> lexer.Tokens.NE) then
                    if (nextTok = lexer.Tokens.Str_Literal) || (nextTok = lexer.Tokens.Bool_Literal) then
                        failwith("Expecting ID or Int, but found " + (string nextTok))
                    let (t3, p3) = expr_value (t2, p2)
                    (t3, p3)
            
            else
                let (t3, p3) = expr_value (t2, p2)
                (t3, p3)
           
        else
            let (t1, p1) = expr_value (tokens, program)
            (t1, p1)
            
    else if (nextToken = lexer.Tokens.Bool_Literal) then
       let (t1, p1) = expr_value (tokens, program)
       let (token1, _) = List.head t1
       if (token1 = lexer.Tokens.LT) || (token1 = lexer.Tokens.GT) || (token1 = lexer.Tokens.NE) || (token1 = lexer.Tokens.Plus) || (token1 = lexer.Tokens.Minus) then
            let (t2, p2) = expr_op (t1 ,p1)
            let (token2, _) = List.head t2
            if (token2 = lexer.Tokens.Bool_Literal) then
                let (t3, p3) = expr_value (t2, p2)
                (t3, p3)
            else
                failwith("Expecting Bool Literal, but found " + (string token2))
       else
            let (t1, p1) = expr_value (tokens, program)
            (t1, p1)
    else
        let (t1, p1) = expr_value (tokens, program)
        (t1, p1)

       
   // -----------------------Condition-part----------------------------------
  and private condition_part (tokens, program) =
    let (T, P) = expr_part (tokens, program)
    
    let (str, valee) = List.head tokens
    let tokstr = string str
    let (s, p) = (List.item 1) tokens
    let (s1, P1) = (List.item 2) tokens
    let ss = string s
    let tokstr1 = string s1
    if (s = lexer.Tokens.EQ) || (s = lexer.Tokens.NE) || (s = lexer.Tokens.LT) || (s = lexer.Tokens.LTE) || (s = lexer.Tokens.GT) || (s = lexer.Tokens.GTE) || (s = lexer.Tokens.Times)
        || (s = lexer.Tokens.Power) then
            
            (T, ["$IF"; tokstr; valee; p; tokstr1; P1]::P)
    else
            (T, ["$IF"; tokstr; valee]::P)

    
  //----------------------------output_value---------------------------
  and private output_value (tokens, program) =
    let (nextToken, _) = List.head tokens
    if (nextToken = lexer.Tokens.Endl) then
        let (T, P) = matchToken lexer.Tokens.Endl (tokens, program)
        
        (T, P)
    else
        let (T1, P1) = expr_value (tokens, program)
        (T1, P1)
    
    //-------------------------Cout------------------------------------
  and private output_part (tokens, program) = 
    let (nextToken, _) = List.head tokens

    if (nextToken = lexer.Tokens.Cout) then
        let (T, P) = matchToken lexer.Tokens.Cout (tokens, program)
        let (T1, P1) = matchToken lexer.Tokens.Output (T, P)
        let (T2, P2) = output_value (T1, P1)
        let (T3, P3) = matchToken lexer.Tokens.Semicolon (T2, P2)

        let (str, vale) = List.head T1
        let tokstr = string str

        (T3, ["$OUTPUT"; tokstr; vale]::P3)
    else
        failwith ("Expecting cout, but found " + (string nextToken))

    //---------------------------Assignment-------------------------
  and private assignment_part (tokens, program) = 
    let (nextToken, _) = List.head tokens
    //
    if (nextToken = lexer.Tokens.ID) then
        let (t, p) = matchToken lexer.Tokens.ID (tokens, program)
        let(t1, p1) = matchToken lexer.Tokens.Assign (t, p)
        let (t2, p2) = expr_part (t1, p1)
        let (t3, p3) = matchToken lexer.Tokens.Semicolon (t2, p2)

        let (str, vale) = List.head tokens
        let ss = string t1
        let (str, valee) = List.head t1
        let tokstr = string str
        let (s, p) = (List.item 1) t1
        let (s1, P1) = (List.item 2) t1
        let ss = string s
        let tokstr1 = string s1

        
        if (s = lexer.Tokens.Plus) || (s = lexer.Tokens.Minus) || (s=lexer.Tokens.Divide) || (s = lexer.Tokens.Times) || (s = lexer.Tokens.Power) then
            (t3, ["$ASSIGN"; vale; tokstr; valee; p; tokstr1; P1]::p3)
        else
            
            (t3, ["$ASSIGN"; vale; tokstr; valee]::p3)
        
        
        
    else
        failwith ("Expecting ID or Assign, SemiColon, but found " + (string nextToken))

  
  //
  // simpleC 
  // 
  
  let private simpleC (tokens, program) =
      let (T1, P1) = matchToken lexer.Tokens.Void (tokens, program)
      let (T2, P2) = matchToken lexer.Tokens.Main (T1, P1)
      let (T3, P3) = matchToken lexer.Tokens.OpenParen (T2, P2)
      let (T4, P4) = matchToken lexer.Tokens.CloseParen (T3, P3)
      let (T5, P5) = matchToken lexer.Tokens.OpenBrace (T4, P4)
      let (T6, P6) = stmts (T5,P5)
      let (T7, P7) = matchToken lexer.Tokens.CloseBrace (T6, P6)
      let (T8, P8) = matchToken lexer.Tokens.EOF (T7, P7)
      (T8, P8)


  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // a tuple containing 3 values:  
  //
  //   (result, msg, program)
  //
  // where result is true or false (legal or not legal), 
  // msg is a success or syntax error message, and program
  // is a list of instructions if parsing was successful.
  //

  let parse tokens =  
    try
      let (_, program) = simpleC (tokens, [])
      (true, "success", List.rev program)
    with 
      | ex -> (false, ex.Message, [])
