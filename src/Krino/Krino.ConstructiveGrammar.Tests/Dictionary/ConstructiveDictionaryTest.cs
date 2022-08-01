using Krino.ConstructiveGrammar.Dictionary;
using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.EnglishDictionary;
using Krino.EnglishGrammar.Syntax;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

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
            var text = myDictionary.Parse("I read book.");
            var grammarStr = text[0].GrammarStr;
            Assert.AreEqual(1, text.Count);
            Assert.AreEqual("i read book.", text[0].Value);
            
            // Simple sentence with suffixes.
            text = myDictionary.Parse("He reads books.");
            grammarStr = text[0].GrammarStr;
            Assert.AreEqual(1, text.Count);
            Assert.AreEqual("he reads books.", text[0].Value);

            // Simple sentence with prefixes.
            text = myDictionary.Parse("I reread antibooks.");
            grammarStr = text[0].GrammarStr;
            Assert.AreEqual(1, text.Count);
            Assert.AreEqual("i reread antibooks.", text[0].Value);

            text = myDictionary.Parse("Cycling is healthy.");
            grammarStr = text[0].GrammarStr;
            Assert.AreEqual(1, text.Count);
            Assert.AreEqual("cycling is healthy.", text[0].Value);
        }

        [Test]
        public void Parse_Argument()
        {
            var texts = myDictionary.Parse("Cycling on the grass is prohibited because walking on the grass is prohibited.");
            Assert.AreEqual(1, texts.Count);
        }
    }
}
