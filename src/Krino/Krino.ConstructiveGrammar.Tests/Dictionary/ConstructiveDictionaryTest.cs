﻿using Krino.ConstructiveGrammar.Dictionary;
using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.EnglishDictionary;
using Krino.EnglishGrammar.Syntax;
using Krino.Vertical.Utils.Diagnostic;
using NUnit.Framework;
using System.Collections.Generic;

using System.Linq;
using System.Threading;

namespace Krino.ConstructiveGrammar.Tests.Dictionary
{
    [TestFixture]
    public class ConstructiveDictionaryTest
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
        public void Parse_SimpleSentence()
        {
            // Simple sentence.
            var text = myDictionary.AnalyzeText("I read book.");
            var grammarStr = text[0].GrammarStr;
            Assert.AreEqual(1, text.Count);
            Assert.AreEqual("i read book.", text[0].Value);
            
            // Simple sentence with suffixes.
            text = myDictionary.AnalyzeText("He reads books.");
            grammarStr = text[0].GrammarStr;
            Assert.AreEqual(1, text.Count);
            Assert.AreEqual("he reads books.", text[0].Value);

            // Simple sentence with prefixes.
            text = myDictionary.AnalyzeText("I reread antibooks.");
            grammarStr = text[0].GrammarStr;
            Assert.AreEqual(1, text.Count);
            Assert.AreEqual("i reread antibooks.", text[0].Value);

            text = myDictionary.AnalyzeText("Cycling is healthy.");
            grammarStr = text[0].GrammarStr;
            Assert.AreEqual(1, text.Count);
            Assert.AreEqual("cycling is healthy.", text[0].Value);
        }

        [Test]
        public void Parse_Argument()
        {
            //Trace.StartProfiler();

            var texts = myDictionary.AnalyzeText("Cycling on the grass is prohibited because walking on the grass is prohibited.");
            Assert.AreEqual(1, texts.Count);

            //Trace.StopProfiler();
            //Thread.Sleep(300);
        }
    }
}
