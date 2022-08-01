using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.ConstructiveGrammar.Morphology;
using NUnit.Framework;

namespace Krino.ConstructiveGrammar.Tests.Morphology
{
    [TestFixture]
    public class AffixBindingTest
    {
        [Test]
        public void TransformAttributes()
        {
            var affixBinding = new AffixBinding()
            {
                AttributesToPick = GrammarAttributes.Morpheme.Free.Lexical.Noun | GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Gerund,
                AttributesToDrop = GrammarAttributes.Morpheme.Free.Lexical,
            };


            var result = affixBinding.TransformAttributes(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            Assert.IsTrue(GrammarAttributes.Morpheme.Free.Lexical.Noun.IsIn(result));
            Assert.IsTrue(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Gerund.IsIn(result));
            Assert.IsFalse(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.IsIn(result));
        }
    }
}
