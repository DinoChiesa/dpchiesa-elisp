' @@BASEFILENAME@@
'
' Description goes here...
'
' Author: @@AUTHOR@@
' Created: @@DATE@@
'
' last saved:
' Time-stamp: <2010-January-14 15:19:45>
' ------------------------------------------------------------------
'
' Copyright (c) 2010 by Dino Chiesa
' All rights reserved!
'
' ------------------------------------------------------------------


Imports System
Imports Ionic.Zip


Namespace Ionic.Tests.Something


Public Class @@BASEFILENAMELESSEXTENSION@@

    ' ==================================================================
    ' Fields
    Private _boolValue As Boolean
    Private _intValue As Integer
    Private _stringValue As String
    Private ReadOnly DefaultIntValue As Integer


    ' ==================================================================
    ' Constructors
    Public Sub New
      ' put default constructor logic here
        Me.DefaultIntValue = 8931
    End Sub


    Public Sub New(ByVal args As String())
        Me.New()
        Dim i As Integer
        For i = 0 To args.Length - 1
            Select Case(args(i))
                Case "-b":
                    If (_boolValue = True) Then
                        Throw New ArgumentException(args(i))
                    End If
                    _boolValue = True

                Case "-s":
                    i += 1
                    If (args.Length <= i) Then
                        Throw New ArgumentException(args(i))
                    End If
                    If Not (Me._stringValue Is Nothing) Then
                        Throw New ArgumentException(args((i - 1)))
                    End If
                    _stringValue = args(i)

                Case "-n":
                    i += 1
                    If (args.Length <= i) Then
                        Throw New ArgumentException(args(i))
                    End If
                    If (Me._intValue <> 0) Then
                        Throw New ArgumentException(args((i - 1)))
                    End If
                    If args(i).StartsWith("0x") Then
                        Me._intValue = Integer.Parse(args(i).Substring(2), NumberStyles.AllowHexSpecifier)
                    Else
                        Me._intValue = Integer.Parse(args(i))
                    End If

                case "-?":
                    Throw New ArgumentException(args(i))

                Case Else:
                    Throw New ArgumentException(args(i))

            End Select
        Next i
        If (Me._intValue = 0) Then
            Me._intValue = Me.DefaultIntValue
        End If
    End Sub


    ' ==================================================================
    ' Methods

    Public Sub Run()

      @@DOT@@

    End Sub


    Public Shared Sub Main(ByVal args As String())
        'If (args.Length < 2) Then
        '     @@BASEFILENAMELESSEXTENSION@@.Usage
        'End If

        Try
            Dim X as New @@BASEFILENAMELESSEXTENSION@@()
            X.Run
        Catch exc1 As Exception
            Console.WriteLine("Exception: {0}", exc1.ToString)
        End Try
    End Sub


    Private Shared Sub Usage()
        Console.WriteLine("usage:" & ChrW(10) & "  @@BASEFILENAMELESSEXTENSION@@ <arg1> [<arg2> | <arg3> ...]")
        Environment.Exit(1)
    End Sub

End Class


End Namespace
