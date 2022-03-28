using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishDictionary;
using Krino.Domain.EnglishGrammar.Parsing;
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
        private IConstructiveDictionary2 myDictionary;
        private EnglishMachine myGrammar;

        [OneTimeSetUp]
        public void Setup()
        {
            //Trace.StartProfiler();

            myDictionary = new EnglishConstructiveDictionaryFactory().Create();
            myGrammar = new EnglishMachine(false);

            //Trace.StopProfiler();
            //Thread.Sleep(300);
        }

        [TearDown]
        public void TearDown()
        {
            myGrammar.Machine.Reset();
        }

        [Test]
        public void Sentence_1_1()
        {
            //Trace.StartProfiler();

            var parser = new Parser(myDictionary, myGrammar.Machine);
            //var results = parser.Parse("I have some good news and some bad news regarding the climate emergency.");
            var results = parser.Parse("I have some good news and some bad news regarding the climate emergency.");

            // Note: 2 - because 'regarding the climate emergency' can be adverbial adjunct or adverbial complement.
            Assert.AreEqual(5, results.Count);
        }

        // I will start with the good news.
        [Test]
        public void Sentence_1_2()
        {
            var parser = new Parser(myDictionary, myGrammar.Machine);

            var results = parser.Parse("I will start with the good news.");

            // AdverbialAdjunct vs AdverbialComplement.
            Assert.AreEqual(2, results.Count);
        }

        // The world, as a small number of people have been saying lately, will not end in 11 years.
        [Test]
        public void Sentence_2_1()
        {
            var parser = new Parser(myDictionary, myGrammar.Machine);

            //var results = parser.Parse("the world");
            var results = parser.Parse("The world as a small number of people have been saying lately will not end in eleven years.");

            // AdverbialAdjunct vs AdverbialComplement.
            Assert.AreEqual(4, results.Count);
        }


        // Students should not have to wear school uniforms because school uniforms infringe upon students right to express their individuality.
        [Test]
        public void Sentence_3()
        {
            var parser = new Parser(myDictionary, myGrammar.Machine);

            //var results = parser.Parse("the world");
            var results = parser.Parse("Students should not have to wear school uniforms because school uniforms infringe upon students right to express their individuality.");

            // AdverbialAdjunct vs AdverbialComplement.
            Assert.AreEqual(81, results.Count);
        }
    }
}
