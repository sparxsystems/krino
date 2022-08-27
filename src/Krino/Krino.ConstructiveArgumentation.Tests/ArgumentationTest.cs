using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.ConstructiveGrammar.Morphology;
using Krino.ConstructiveGrammar.Syntax;
using Krino.EnglishGrammar.Morphology;
using Krino.EnglishGrammar.Syntax;
using NUnit.Framework;
using System.Linq;

namespace Krino.ConstructiveArgumentation.Tests
{
    [TestFixture]
    public class ArgumentationTest
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
        public void IsArgument()
        {
            var it = new Word(myMorphology, "it", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var is_ = new Word(myMorphology, "is", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.ThirdPerson);
            var wrong = new Word(myMorphology, "wrong", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var because = new Word(myMorphology, "because", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating.Sememe.Cause);
            var incorrect = new Word(myMorphology, "incorrect", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var punct = new Word(myMorphology, ".", GrammarAttributes.PunctuationMark.Period);

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