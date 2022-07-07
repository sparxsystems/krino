using Krino.ConstructiveGrammar.AdTrees;
using Krino.ConstructiveGrammar.Dictionary;
using Krino.ConstructiveGrammar.Syntax;
using Krino.EnglishDictionary;
using Krino.EnglishGrammar.Parsing;
using Krino.Vertical.Utils.Diagnostic;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.GretaTest
{
    [TestFixture]
    public class GretaTest
    {
        private IConstructiveDictionary myDictionary;

        [OneTimeSetUp]
        public void Setup()
        {
            //Trace.StartProfiler();

            myDictionary = new EnglishConstructiveDictionaryFactory().Create();

            //Trace.StopProfiler();
            //Thread.Sleep(300);
        }

        [Test]
        public void Sentence_1_1()
        {
            //Trace.StartProfiler();

            //var results = parser.Parse("I have some good news and some bad news regarding the climate emergency.");
            var results = myDictionary.Parse("I have some good news and some bad news regarding the climate emergency.");

            // Note: 2 - because 'regarding the climate emergency' can be adverbial adjunct or adverbial complement.
            Assert.AreEqual(5, results.Count);
        }

        // I will start with the good news.
        [Test]
        public void Sentence_1_2()
        {
            var results = myDictionary.Parse("I will start with the good news.");

            // AdverbialAdjunct vs AdverbialComplement.
            Assert.AreEqual(2, results.Count);
        }

        // The world, as a small number of people have been saying lately, will not end in 11 years.
        [Test]
        public void Sentence_2_1()
        {
            //var results = parser.Parse("the world");
            var results = myDictionary.Parse("The world as a small number of people have been saying lately will not end in eleven years.");

            // AdverbialAdjunct vs AdverbialComplement.
            Assert.AreEqual(4, results.Count);
        }


        // Students should not have to wear school uniforms because school uniforms infringe upon students right to express their individuality.
        [Test]
        public void Sentence_3()
        {
            //var results = parser.Parse("the world");
            var results = myDictionary.Parse("Students should not have to wear school uniforms because school uniforms infringe upon students right to express their individuality.");

            // AdverbialAdjunct vs AdverbialComplement.
            Assert.AreEqual(81, results.Count);
        }
    }
}
