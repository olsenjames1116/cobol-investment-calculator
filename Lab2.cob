      *      James Olsen
      *      Program that accepts an investment amount, annual interest rate, number of months, and any additional yearly
      *      investment for the investment and returns the monthly amount, gain from interest, additional investment, and
      *      a balance summary.
      
       Identification Division.
       Program-ID.  Lab2.
           
       Data Division.
       Working-Storage Section.
       01   InvestAmt       Pic S9(9)V9(2).
       01   TempInvestAmt   Pic S9(9)V9(2).
       01   IntRate         Pic S9(3)V9(2).
       01   NumMonths       Pic S9(4).
       78   NewLine         Value x"0a".
       01   MonthCount      Pic 9(4) Value 0.
       01   MonthString     Pic Z(5).
       01   InvAmtString    Pic $$$$$,$$$,$$9.99.
       01   Interest        Pic 9(8)V9(2).
       01   InterestString  Pic $$$$,$$$,$$9.99.
       01   SumInvAmtStr    Pic $$$$,$$$,$$9.99.
       01   SumIntRateStr   Pic Z(13)9.999.
       01   SumNumMonthStr  Pic Z(25)9.
       01   TotalInt        Pic 9(7)V9(2).
       01   SumTotIntStr    Pic $(8)$$$,$$9.99.
       01   FinalInvAmt     Pic 9(9)V9(2).
       01   FinalInvAmtStr  Pic $(3)$,$$$,$$$,$$9.99.
       01   AddInvAmt       Pic S9(9)V9(2).
       01   AddInvStr       Pic $$$,$$$,$$9.99.
       01   YearCount       Pic 999.
       
       Procedure Division.
       000-Main.
           Display "Enter Investment Amount: " With No Advancing
           Accept InvestAmt
           
           If InvestAmt<0
                Perform until InvestAmt>=0
                    Display "Investment Amount must be positive"
                    Display "Enter Investment Amount: " With No
                    Advancing
                    Accept InvestAmt
                End-Perform
           End-If
           
           Move InvestAmt to TempInvestAmt
           Display "Enter Annual Interest Rate: " With No Advancing
           Accept IntRate
           
           If IntRate<0
                Perform until IntRate>=0
                    Display "Annual Interest Rate must be positive"
                    Display "Enter Annual Interest Rate: " With No
                    Advancing
                    Accept IntRate
                End-Perform
           End-If
           
           Display "Enter Number of Months: " With No Advancing
           Accept NumMonths
           
           If NumMonths<0
                Perform until NumMonths>=0
                    Display "Number of Months must be positive"
                    Display "Enter Number of Months: " With No
                    Advancing
                    Accept NumMonths
                End-Perform
            End-If
            
            Display "Enter Additional Investment Amount: " With No
            Advancing
            Accept AddInvAmt
            
            If AddInvAmt<0
                Perform until AddInvAmt>=0
                    Display "Additional Investment Amount must be" &
                    " positive"
                    Display "Enter Additional Investment Amount: "
                    With No Advancing
                    Accept AddInvAmt
                End-Perform
           End-If
            
            Display NewLine
            Display "Investment Schedule:"
            Display NewLine
            Display "Month     Beg Balance        Interest   Additional"
            
            Perform until MonthCount>=NumMonths
                Add 1 to MonthCount
                Move MonthCount to MonthString
                Display MonthString With No Advancing
                Compute TempInvestAmt = Interest + TempInvestAmt
                Compute Interest Rounded = ((IntRate/100)*
                TempInvestAmt)/(12) 
                Compute TotalInt = TotalInt + Interest
                Move TempInvestAmt to InvAmtString
                Display InvAmtString With No Advancing
                Move Interest to InterestString
                Display InterestString With No Advancing
                
                If function Mod(MonthCount,13)=0
                    Move AddInvAmt to AddInvStr
                    Compute TempInvestAmt = AddInvAmt + TempInvestAmt
                    Add 1 to YearCount
                Else
                    Move 0 to AddInvStr
                End-If
                
                Display AddInvStr
            End-Perform
            
            Display NewLine
            Display "Balance Summary:"
            Display NewLine
            Move InvestAmt to SumInvAmtStr
            Display "Investment Amount" SumInvAmtStr
            Move IntRate to SumIntRateStr
            Display "Interest Rate" SumIntRateStr "%"
            Move NumMonths to SumNumMonthStr
            Display "Months" SumNumMonthStr
            Move TotalInt to SumTotIntStr
            Display "Total Interest" SumTotIntStr
            Compute FinalInvAmt = TotalInt + InvestAmt + AddInvAmt *
            YearCount
            Move FinalInvAmt to FinalInvAmtStr
            Display "Final Balance" FinalInvAmtStr
            
            Stop Run.
           