using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.ConstructiveGrammar.Parsing;
using Krino.EnglishGrammar.Parsing;
using NUnit.Framework;
using System.Linq;

namespace Krino.ConstructiveGrammar.Tests.Parsing
{
    [TestFixture]
    public class StatePathExtTest
    {
        private GrammarMachine myGrammar;

        [OneTimeSetUp]
        public void Setup()
        {
            //Trace.StartProfiler();

            var english = new EnglishMachine(true).Machine;
            myGrammar = new GrammarMachine(english);

            //Trace.StopProfiler();
            //Thread.Sleep(300);
        }

        [TearDown]
        public void TearDown()
        {
            myGrammar.Reset();
        }

        [Test]
        public void GetText()
        {
            var i = new Word("i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var read = new Word("read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var the = new Word("the", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            var book = new Word("book", GrammarAttributes.Morpheme.Free.Lexical.Noun);

            var give = new Word("give", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent);
            var him = new Word("him", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var a = new Word("a", GrammarAttributes.Morpheme.Free.Functional.Determiner);

            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);


            _ = myGrammar.DebugView;

            // First sentence.
            myGrammar.Add(i);
            myGrammar.Add(read);
            myGrammar.Add(the);
            myGrammar.Add(book);
            myGrammar.Add(punct);

            // Second sentence.
            myGrammar.Add(i);
            myGrammar.Add(give);
            myGrammar.Add(him);
            myGrammar.Add(a);
            myGrammar.Add(book);
            myGrammar.Add(punct);


            // Although the sentence is not completed the text structure needs to contain all available elements.
            myGrammar.Add(i);

            var texts = myGrammar.ActiveStates.Select(x => x.Path.GetText()).ToList();
            var text = texts.FirstOrDefault();
            Assert.AreEqual("i read the book. i give him a book. i", text.Value);
        }
    }
}
