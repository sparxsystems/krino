using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Morphemes.StructuralAttributesArrangement
{
    [TestFixture]
    public class StructuralAttributesTest
    {
        [Test]
        public void GetGrammarCharacter()
        {
            Assert.AreEqual(GrammarCharacter.O, StructuralAttributes.GetGrammarCharacter(StructuralAttributes.O));
            Assert.AreEqual(GrammarCharacter.I, StructuralAttributes.GetGrammarCharacter(StructuralAttributes.I));
            Assert.AreEqual(GrammarCharacter.A, StructuralAttributes.GetGrammarCharacter(StructuralAttributes.A));
            Assert.AreEqual(GrammarCharacter.E, StructuralAttributes.GetGrammarCharacter(StructuralAttributes.E));
            Assert.AreEqual(GrammarCharacter.U, StructuralAttributes.GetGrammarCharacter(StructuralAttributes.U));

            Assert.AreEqual(GrammarCharacter.Epsilon, StructuralAttributes.GetGrammarCharacter(0));
        }
    }
}
