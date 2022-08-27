using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.ConstructiveGrammar.Syntax;
using Krino.EnglishGrammar.Morphology;
using Krino.EnglishGrammar.Syntax;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Krino.ConstructiveArgumentation.Tests
{
    [TestFixture]
    public class ArgumentTest
    {
        private SyntaxReader myGrammar;

        [OneTimeSetUp]
        public void Setup()
        {
            //Trace.StartProfiler();

            var english = new EnglishMachine(true).Machine;
            myGrammar = new SyntaxReader(english);

            //Trace.StopProfiler();
            //Thread.Sleep(300);
        }

        [TearDown]
        public void TearDown()
        {
            myGrammar.Reset();
        }

        [Test]
        public void Premise_Conclusion_Form()
        {
            var morphology = new EnglishMorphology();

            var it = new Word(morphology, "it", GrammarAttributes.Morpheme.Free.Functional.Pronoun);
            var is_ = new Word(morphology, "is", GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.ThirdPerson);
            var wrong = new Word(morphology, "wrong", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var because = new Word(morphology, "because", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating.Sememe.Cause);
            var incorrect = new Word(morphology, "incorrect", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            var punct = new Word(morphology, ".", GrammarAttributes.PunctuationMark.Period);

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

            var argument = new Argument(texts[0].Sentences[0]);
            Assert.AreEqual("it is wrong", argument.Conclusion.Value);
            Assert.AreEqual("because it is incorrect", argument.Premise.Value);
            Assert.AreEqual(ArgumentForm.a_is_X_because_a_is_Y, argument.Form);
        }
    }
}
