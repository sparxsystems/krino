using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Parsing
{
    [TestFixture]
    public class AdTreeBuilderTest
    {
        [Test]
        public void AddMorpheme_FindingPatternForMorpheme()
        {
            List<IPattern> patterns = new List<IPattern>()
            {
                new Pattern()
                {
                    MorphemeMatchingRule = MorphemeRule.O,
                    AdPositionMatchingRule = PatternRule.Epsilon,
                    RightMatchingRule = PatternRule.Nothing,
                    LeftMatchingRule = PatternRule.Nothing,
                },
            };

            AdTreeBuilder builder = new AdTreeBuilder(patterns);

            Morpheme morpheme = new Morpheme("I") { Attributes = StructuralAttributes.O.Pronoun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));


            patterns = new List<IPattern>()
            {
                new Pattern()
                {
                    MorphemeMatchingRule = MorphemeRule.O,
                    AdPositionMatchingRule = PatternRule.Epsilon,
                    RightMatchingRule = PatternRule.Nothing,
                    LeftMatchingRule = PatternRule.Nothing,
                },
            };

            builder = new AdTreeBuilder(patterns);

            morpheme = new Morpheme("read") { Attributes = StructuralAttributes.I.Verb };
            Assert.IsFalse(builder.AddMorpheme(morpheme));
        }

        [Test]
        public void AddMorpheme_FindingPatternForAdTreeElement()
        {
            List<IPattern> patterns = new List<IPattern>()
            {
                // Noun
                new Pattern()
                {
                    MorphemeMatchingRule = MorphemeRule.O,
                    AdPositionMatchingRule = PatternRule.Epsilon,
                    RightMatchingRule = PatternRule.Nothing,
                    LeftMatchingRule = PatternRule.Nothing,
                },

                // Epsilon
                new Pattern()
                {
                    MorphemeMatchingRule = MorphemeRule.Epsilon,
                    AdPositionMatchingRule = PatternRule.Anything,
                    RightMatchingRule = PatternRule.Anything,
                    LeftMatchingRule = PatternRule.Anything,
                },
            };

            AdTreeBuilder builder = new AdTreeBuilder(patterns);

            Morpheme morpheme = new Morpheme("I") { Attributes = StructuralAttributes.O.Pronoun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            // Adding the morpheme which match to the adposition.
            morpheme = new Morpheme("") { Attributes = StructuralAttributes.Epsilon };
            Assert.IsTrue(builder.AddMorpheme(morpheme));
            Assert.AreEqual(1, builder.ActiveAdTrees.Count());
            Assert.AreEqual(2, builder.ActiveAdTrees[0].Count());



            builder = new AdTreeBuilder(patterns);

            morpheme = new Morpheme("I") { Attributes = StructuralAttributes.O.Pronoun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            // Adding the morpheme which does not match.
            morpheme = new Morpheme("book") { Attributes = StructuralAttributes.O.Noun };
            Assert.IsFalse(builder.AddMorpheme(morpheme));
            Assert.AreEqual(1, builder.ActiveAdTrees.Count());
            Assert.AreEqual(1, builder.ActiveAdTrees[0].Count());
        }

        [Test]
        public void AddMorpheme_I_read_the_book()
        {
            List<IPattern> patterns = new List<IPattern>()
            {
                new Pattern()
                {
                    MorphemeMatchingRule = MorphemeRule.O,
                    AdPositionMatchingRule = PatternRule.Epsilon,
                    RightMatchingRule = PatternRule.Nothing,
                    LeftMatchingRule = PatternRule.Nothing,
                },
                new Pattern()
                {
                    MorphemeMatchingRule = MorphemeRule.O,
                    AdPositionMatchingRule = PatternRule.Epsilon,
                    RightMatchingRule = PatternRule.Nothing,
                    LeftMatchingRule = PatternRule.Nothing,
                    GovernorMatchingRule = new PatternRule(new MorphemeRule(null, null, StructuralAttributes.I.Verb.Bivalent, 0), 0, 0),
                },
                new Pattern()
                {
                    MorphemeMatchingRule = MorphemeRule.I2,
                    AdPositionMatchingRule = PatternRule.Epsilon,
                    RightMatchingRule = PatternRule.Nothing,
                    LeftMatchingRule = PatternRule.Nothing,
                },
                new Pattern()
                {
                    MorphemeMatchingRule = MorphemeRule.A,
                    AdPositionMatchingRule = PatternRule.Epsilon,
                    RightMatchingRule = PatternRule.Nothing,
                    LeftMatchingRule = PatternRule.Nothing,
                },

                new Pattern()
                {
                    MorphemeMatchingRule = MorphemeRule.Epsilon,
                    AdPositionMatchingRule = PatternRule.Anything,
                    RightMatchingRule = PatternRule.Anything,
                    LeftMatchingRule = PatternRule.Anything,
                },
                new Pattern()
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.First,

                    MorphemeMatchingRule = MorphemeRule.Epsilon,
                    AdPositionMatchingRule = PatternRule.Anything,
                    RightMatchingRule = PatternRule.Anything,
                    LeftMatchingRule = PatternRule.Anything,
                },
            };

            AdTreeBuilder builder = new AdTreeBuilder(patterns);


            Morpheme morpheme = new Morpheme("I") { Attributes = StructuralAttributes.O.Pronoun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));
            Assert.AreEqual(2, builder.ActiveAdTrees.Count);
            Assert.IsTrue(builder.ActiveAdTrees[0].Pattern == patterns[0]);
            Assert.IsTrue(builder.ActiveAdTrees[1].Pattern == patterns[1]);

            morpheme = new Morpheme("") { Attributes = StructuralAttributes.Epsilon };
            Assert.IsTrue(builder.AddMorpheme(morpheme));
            Assert.AreEqual(4, builder.ActiveAdTrees.Count);
            Assert.IsTrue(builder.ActiveAdTrees[0].Pattern == patterns[4]);
            Assert.IsTrue(builder.ActiveAdTrees[1].Pattern == patterns[5]);
            Assert.IsTrue(builder.ActiveAdTrees[2].Pattern == patterns[4]);
            Assert.IsTrue(builder.ActiveAdTrees[3].Pattern == patterns[5]);

            morpheme = new Morpheme("read") { Attributes = StructuralAttributes.I.Verb.Bivalent };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("") { Attributes = StructuralAttributes.Epsilon };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("the") { Attributes = StructuralAttributes.A.Determiner.DefiniteArticle };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("") { Attributes = StructuralAttributes.Epsilon };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("book") { Attributes = StructuralAttributes.O.Noun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));
        }
    }
}
