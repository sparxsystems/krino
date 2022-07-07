using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.ConstructiveGrammar.Syntax;
using Krino.EnglishGrammar.Parsing;
using NUnit.Framework;
using System.Linq;

namespace Krino.ConstructiveArgumentation.Tests
{
    [TestFixture]
    public class ArgumentationTest
    {
        private SyntaxMachine myGrammar;

        [OneTimeSetUp]
        public void Setup()
        {
            //Trace.StartProfiler();

            var english = new EnglishMachine(true).Machine;
            myGrammar = new SyntaxMachine(english);

            //Trace.StopProfiler();
            //Thread.Sleep(300);
        }

        [TearDown]
        public void TearDown()
        {
            myGrammar.Reset();
        }


        [Test]
        public void IsArgument()
        {
            var it = new Word("it", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var is_ = new Word("is", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.ThirdPerson);
            var wrong = new Word("wrong", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var because = new Word("because", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating.Sememe.Cause);
            var incorrect = new Word("incorrect", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var punct = new Word(".", GrammarAttributes.PunctuationMark.Period);

            _ = myGrammar.DebugView;

            myGrammar.Add(it);
            myGrammar.Add(is_);
            myGrammar.Add(wrong);
            myGrammar.Add(because);
            myGrammar.Add(it);
            myGrammar.Add(is_);
            myGrammar.Add(incorrect);
            myGrammar.Add(punct);

            var texts = myGrammar.GetTexts().ToList();
            Assert.AreEqual(1, texts.Count);
            Assert.AreEqual(1, texts[0].Sentences.Count);

            var argumentation = new Argumentation();
            Assert.IsTrue(argumentation.IsArgument(texts[0].Sentences[0]));
        }
    }
}