using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using NUnit.Framework;

namespace Krino.Domain.EnglishGrammar.Tests.LinguisticConstructions
{
    [TestFixture]
    public class EnglishPatternTest
    {
        [Test]
        public void CompoundSubject()
        {
            var parent = EnglishPattern.O1_I;
            var child = EnglishPattern.O_E_O;
            Assert.IsTrue(parent.LeftPatternRule.Evaluate(child));
        }

        [Test]
        public void SimpleFuture()
        {
            var parent = EnglishPattern.O1_I;
            var child = EnglishPattern.Will_I;
            Assert.IsTrue(parent.RightPatternRule.Evaluate(child));

            parent = EnglishPattern.Will_I;
            child = EnglishPattern.I_Lexeme_Verb_Will;
            Assert.IsTrue(parent.LeftPatternRule.Evaluate(child));

            parent = EnglishPattern.Will_I;
            child = EnglishPattern.I_Lexeme_Verb_Will;
            Assert.IsTrue(parent.LeftPatternRule.Evaluate(child));

            parent = EnglishPattern.Will_I;
            child = EnglishPattern.I_Lexeme_Verb;
            Assert.IsFalse(parent.LeftPatternRule.Evaluate(child));
        }

        [Test]
        public void PresentPerfectContinuous()
        {
            var parent = EnglishPattern.O1_I;
            var child = EnglishPattern.Have_Been_I_ing;
            Assert.IsTrue(parent.RightPatternRule.Evaluate(child));

            parent = EnglishPattern.Have_Been_I_ing;
            child = EnglishPattern.Have_Been_Auxiliary;
            Assert.IsTrue(parent.LeftPatternRule.Evaluate(child));

            parent = EnglishPattern.Have_Been_Auxiliary;
            child = EnglishPattern.I_Lexeme_Verb_Have;
            Assert.IsTrue(parent.LeftPatternRule.Evaluate(child));

            parent = EnglishPattern.Have_Been_Auxiliary;
            child = EnglishPattern.I_Lexeme_Verb_Been;
            Assert.IsTrue(parent.RightPatternRule.Evaluate(child));

            parent = EnglishPattern.Have_Been_I_ing;
            child = EnglishPattern.I_to_I_ing;
            Assert.IsTrue(parent.RightPatternRule.Evaluate(child));


            parent = EnglishPattern.Have_Been_Auxiliary;
            child = EnglishPattern.I_Lexeme_Verb;
            Assert.IsFalse(parent.LeftPatternRule.Evaluate(child));

            parent = EnglishPattern.Have_Been_Auxiliary;
            child = EnglishPattern.I_Lexeme_Verb;
            Assert.IsFalse(parent.RightPatternRule.Evaluate(child));

            parent = EnglishPattern.Have_Been_I_ing;
            child = EnglishPattern.I_Lexeme_Verb;
            Assert.IsFalse(parent.RightPatternRule.Evaluate(child));
        }
    }
}
