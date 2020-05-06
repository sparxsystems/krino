using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Morphemes.StructuralAttributesArrangement
{
    [TestFixture]
    public class GrammarCharacterExtTest
    {
        [Test]
        public void GetGrammarCharacter()
        {
            Assert.AreEqual(GrammarCharacter.O, GrammarCharacterExt.GetGrammarCharacter(StructuralAttributes.O));
            Assert.AreEqual(GrammarCharacter.I, GrammarCharacterExt.GetGrammarCharacter(StructuralAttributes.I));
            Assert.AreEqual(GrammarCharacter.A, GrammarCharacterExt.GetGrammarCharacter(StructuralAttributes.A));
            Assert.AreEqual(GrammarCharacter.E, GrammarCharacterExt.GetGrammarCharacter(StructuralAttributes.E));
            Assert.AreEqual(GrammarCharacter.U, GrammarCharacterExt.GetGrammarCharacter(StructuralAttributes.U));

            Assert.AreEqual(GrammarCharacter.Epsilon, GrammarCharacterExt.GetGrammarCharacter(0));
        }
    }
}
