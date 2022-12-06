using Nancy.Json;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using RestSharp;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Net.Http.Json;
using System.Security.Cryptography;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace ConsoleAppForSmallThings
{
    class Program
    {
        static async Task Main(string[] args)
        {

            long[,] horizontal = new long[3, 3] { { 0, 1, 0 }, { 0, 1, 0}, { 0, 0, 0 } };

            string positionOfHArc = "";


            int rowLength = horizontal.GetLength(0);
            int colLength = horizontal.GetLength(1);

            for (int i = 0; i < rowLength; i++)
            {
                for (int j = 0; j < colLength; j++)
                {
                    if (horizontal[i, j] == 1)
                        positionOfHArc = i.ToString() + j.ToString();

                    Console.Write(string.Format("{0} ", horizontal[i, j]));
                }
                Console.Write("|");
            }

            Console.Write(Environment.NewLine + Environment.NewLine);

            long[,] vertical = new long[4, 2] { { 0, 0 }, { 1, 0 }, { 1, 0 }, { 0, 0 } };

            int rowLengthV = vertical.GetLength(0);
            int colLengthV = vertical.GetLength(1);

            for (int i = 0; i < rowLengthV; i++)
            {
                for (int j = 0; j < colLengthV; j++)
                {
                    Console.Write(string.Format("{0} ", vertical[i, j]));
                }
                Console.Write("|");
            }
            Console.ReadLine();

            bool twoStraightHorizontal = false;
            bool twoStraightVertical = false;
            bool topLeftHorizontalAndVertical = false;

            Console.Write(Environment.NewLine + Environment.NewLine);

            int BoxesCount = 0;

            for (int i = 0; i < rowLength - 1; i++)
            {
                for (int j = 0; j < rowLengthV - 1; j++)
                {
                    if (j < 3)
                        Console.Write(string.Format("{0} ", horizontal[i, j]));


                    Console.Write(string.Format("{0} ", vertical[j, i]));

                    if (i + 1 < 3)
                    {
                        if (horizontal[i, j] == 1 && horizontal[i+1,j] == 1)
                            twoStraightHorizontal = true;
                    }
                        if (j+1 < 4)
                    {
                        if (vertical[j, i] == 1 && vertical[j+1, i] == 1)
                            twoStraightVertical = true;

                    }

                    if (horizontal[i, j] == 1 && vertical[j, i] == 1)
                        topLeftHorizontalAndVertical = true;

                    Console.Write("|");

                    if (twoStraightHorizontal && twoStraightVertical && topLeftHorizontalAndVertical)
                    {
                        BoxesCount++;
                        twoStraightHorizontal = false;
                        twoStraightVertical = false;
                        topLeftHorizontalAndVertical = false;
                    }

                }
                Console.Write(Environment.NewLine + Environment.NewLine);

                Console.ReadLine();

              

            }

            Console.WriteLine("We have this boxes: " + BoxesCount);


        }

        public static int factorial(int number)
        {
            if (number == 0)
                return 1;
            else
                return number* factorial(number-1);
        }

    }
}
