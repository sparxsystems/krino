using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Morphemes
{
    [TestFixture]
    public class MorphemeEqualityComparerTest
    {
        [Test]
        public void Equals()
        {
            MorphemeEqualityComparer comparer = new MorphemeEqualityComparer();
            
            Assert.IsTrue(comparer.Equals(
                new Morpheme("room") { Attributes = StructuralAttributes.O.Noun }, 
                new Morpheme("room") { Attributes = StructuralAttributes.O.Noun }));

            Assert.IsFalse(comparer.Equals(
                new Morpheme("bla") { Attributes = StructuralAttributes.O.Noun },
                new Morpheme("room") { Attributes = StructuralAttributes.O.Noun }));

            Assert.IsFalse(comparer.Equals(
                new Morpheme("room") { Attributes = StructuralAttributes.O.Noun },
                new Morpheme("room") { Attributes = StructuralAttributes.O.Noun.Countable }));
        }
    }
}
