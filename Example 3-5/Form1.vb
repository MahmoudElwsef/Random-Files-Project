Public Structure Hagz
    'Trip Data
    Dim TripNo As Integer
    Dim TripDate, TripTime, ArriveTime, City1, City2 As String

    'Passenger Data
    <VBFixedArray(55)> Dim korsi() As Integer    'used to store korsi status
    <VBFixedArray(55)> Dim passenger() As String
    <VBFixedArray(55)> Dim phone() As String
    <VBFixedArray(55)> Dim address() As String
End Structure

Public Class Form1

    'Global Variables
    Dim buttons(55) As Button 'Array Of B uttons (56 Button)
    Public Prec As Hagz
    Dim z, s, x, j, position As Integer

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        FileOpen(1, "ProjectData.txt", OpenMode.Random,,, Len(Prec))

        Prec.korsi = New Integer(55) {}
        Prec.passenger = New String(55) {}
        Prec.phone = New String(55) {}
        Prec.address = New String(55) {}
        Label2.Text = Now() 'Show Time and Date

        'send Handler To korsyy_no
        For i = 1 To 56
            buttons(i - 1) = Me.Controls("Button" & i)
            AddHandler buttons(i - 1).Click, AddressOf korsyy_no
        Next

    End Sub

    Private Sub BtnNewTrip_Click(sender As Object, e As EventArgs) Handles BtnNewTrip.Click

        position = Loc(1)
        Seek(1, Val(TextBox1.Text))

        Prec.TripNo = Val(TextBox1.Text)
        Prec.TripDate = TextBox2.Text
        Prec.City1 = TextBox3.Text
        Prec.TripTime = TextBox4.Text
        Prec.ArriveTime = TextBox5.Text
        Prec.City2 = TextBox6.Text

        For i = 0 To 55
            Prec.korsi(i) = 0
            Prec.passenger(i) = "0"
            Prec.phone(i) = "0"
            Prec.address(i) = "0"
        Next

        s = Val(TextBox1.Text)
        FilePut(1, Prec, s)
        ClearTripData()

    End Sub

    Private Sub BtnBooking_Click(sender As Object, e As EventArgs) Handles BtnBooking.Click

        ViewTripData()
        ViewButtonsColors()

        If z = 0 Then

            x = MsgBox("هل تريد حجز الكرسي ؟", MsgBoxStyle.OkCancel)
            If x = 1 Then

                buttons(j).BackColor = Color.Orange
                z = 1

                FileGet(1, Prec, Val(TextBox1.Text))
                Prec.korsi(j) = z
                Prec.passenger(j) = TextBox7.Text
                Prec.phone(j) = TextBox8.Text
                Prec.address(j) = TextBox9.Text

                s = Val(TextBox1.Text)
                FilePut(1, Prec, s)

            ElseIf x = 2 Then

                buttons(j).BackColor = Color.Lime
                Exit Sub

            End If

        ElseIf z = 1 Then

            x = MsgBox("هذا الكرسي محجوز بالفعل ", MsgBoxStyle.Critical)
            ClearPassengerData()
            Exit Sub

        ElseIf z = 2 Then

            x = MsgBox(" هذا الكرسي محجوز ومؤكد حجزه ", MsgBoxStyle.Critical)
            ClearPassengerData()
            Exit Sub

        End If

        ClearPassengerData()

    End Sub

    Private Sub BtnConfirmBook_Click(sender As Object, e As EventArgs) Handles BtnConfirmBook.Click

        ViewTripData()
        ViewButtonsColors()

        If z = 0 Then
            MsgBox("هذا الكرسي لم يسبق حجزه", MsgBoxStyle.Critical)

        ElseIf z = 1 Then
            x = MsgBox("هل تريد تأكيد حجز الكرسي ؟", MsgBoxStyle.OkCancel)

            If x = 1 Then

                buttons(j).BackColor = Color.Red
                z = 2

                FileGet(1, Prec, Val(TextBox1.Text))
                Prec.korsi(j) = z
                s = Val(TextBox1.Text)

                FilePut(1, Prec, s)

            ElseIf x = 2 Then

                buttons(j).BackColor = Color.Orange
                Exit Sub

            End If

        ElseIf z = 2 Then
            x = MsgBox(" هذا الكرسي محجوز ومؤكد حجزه ", MsgBoxStyle.Critical)
            ClearPassengerData()

        End If

        ClearPassengerData()

    End Sub

    Private Sub BtnDeleteBook_Click(sender As Object, e As EventArgs) Handles BtnDeleteBook.Click

        ViewTripData()
        ViewButtonsColors()

        FileGet(1, Prec, Val(TextBox1.Text))

        If z = 0 Then
            MsgBox("هذا الكرسي لم يسبق حجزه", MsgBoxStyle.Critical)

        ElseIf z = 1 Then
            x = MsgBox("هل تريد الغاء الحجز ؟", MsgBoxStyle.OkCancel)

            If x = 1 Then

                buttons(j).BackColor = Color.Lime
                z = 0
                Prec.korsi(j) = 0
                Prec.passenger(j) = "0"
                Prec.phone(j) = "0"
                Prec.address(j) = "0"

                s = Val(TextBox1.Text)
                FilePut(1, Prec, s)

            ElseIf x = 2 Then

                buttons(j).BackColor = Color.Orange
                Exit Sub

            End If

        ElseIf z = 2 Then
            MsgBox("لا يمكن الغاء الحجز", MsgBoxStyle.Critical)

        End If

        ClearPassengerData()

    End Sub

    Private Sub BtnExit_Click(sender As Object, e As EventArgs) Handles BtnExit.Click
        FileClose(1)
        End
    End Sub

    Private Sub korsyy_no(ByVal sender As System.Object, ByVal e As System.EventArgs)

        FileGet(1, Prec, Val(TextBox1.Text))

        'return Button That User Clicked
        For i = 0 To (buttons.Length - 1)
            If buttons(i) Is sender Then
                j = i
                Exit For
            End If
        Next

        z = Prec.korsi(j)
        If z <> 0 Then

            TextBox7.Text = Prec.passenger(j)
            TextBox8.Text = Prec.phone(j)
            TextBox9.Text = Prec.address(j)

        End If

        If j <= 8 Then
            Label13.Text = "Firist Class"
        Else
            Label13.Text = "Second Class"
        End If

    End Sub

    Sub ViewTripData()

        FileGet(1, Prec, Val(TextBox1.Text))

        TextBox2.Text = Prec.TripDate
        TextBox3.Text = Prec.City1
        TextBox4.Text = Prec.TripTime
        TextBox5.Text = Prec.ArriveTime
        TextBox6.Text = Prec.City2

    End Sub

    Sub ViewButtonsColors()

        FileGet(1, Prec, Val(TextBox1.Text))

        If Prec.TripNo <> Val(TextBox1.Text) Then

            FileClose(1)
            Exit Sub

        End If

        For i = 0 To 55

            If Prec.korsi(i) = 1 Then
                buttons(i).BackColor = Color.Orange

            ElseIf Prec.korsi(i) = 2 Then
                buttons(i).BackColor = Color.Red

            Else
                buttons(i).BackColor = Color.Lime
            End If

        Next

    End Sub


    Sub ClearTripData()

        TextBox1.Clear()
        TextBox2.Clear()
        TextBox3.Clear()
        TextBox4.Clear()
        TextBox5.Clear()
        TextBox6.Clear()

    End Sub

    Sub ClearPassengerData()

        TextBox7.Clear()
        TextBox8.Clear()
        TextBox9.Clear()
        Label13.Text = ""

    End Sub

    Private Sub TextBox1_TextChanged(sender As Object, e As EventArgs) Handles TextBox1.TextChanged

        Try

            ViewTripData()
            ViewButtonsColors()

        Catch ex As Exception

            MsgBox("No Trip Exist Under This Number", MsgBoxStyle.Critical)

        End Try

    End Sub

End Class
