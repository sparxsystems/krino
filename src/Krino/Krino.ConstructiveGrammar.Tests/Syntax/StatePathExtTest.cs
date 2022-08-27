using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.ConstructiveGrammar.Morphology;
using Krino.ConstructiveGrammar.Syntax;
using Krino.EnglishGrammar.Morphology;
using Krino.EnglishGrammar.Syntax;
using NUnit.Framework;
using System.Linq;

namespace Krino.ConstructiveGrammar.Tests.Syntax
{
    [TestFixture]
    public class StatePathExtTest
    {
        private SyntaxReader myGrammar;
        private IMorphology myMorphology;

        [OneTimeSetUp]
        public void Setup()
        {
            //Trace.StartProfiler();

            var english = new EnglishMachine(true).Machine;
            myGrammar = new SyntaxReader(english);
            myMorphology = new EnglishMorphology();

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
            var i = new Word(myMorphology, "i", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var read = new Word(myMorphology, "read", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            var the = new Word(myMorphology, "the", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            var book = new Word(myMorphology, "book", GrammarAttributes.Morpheme.Free.Lexical.Noun);

            var give = new Word(myMorphology, "give", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base | GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent);
            var him = new Word(myMorphology, "him", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var a = new Word(myMorphology, "a", GrammarAttributes.Morpheme.Free.Functional.Determiner);

            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);


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
