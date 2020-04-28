using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
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
        public void AddMorpheme_I_read()
        {
            List<IPattern> patterns = new List<IPattern>()
            {
                new Pattern()
                {
                    MorphemeRule = MorphemeRule.O,
                    AdPositionRule = PatternRule.Anything,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern()
                {
                    MorphemeRule = MorphemeRule.I,
                    AdPositionRule = PatternRule.Anything,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern()
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.First,
                    MorphemeRule = MorphemeRule.Epsilon,
                    AdPositionRule = PatternRule.Anything,
                    RightRule = PatternRule.Anything.Where(MorphemeRule.I),
                    LeftRule = PatternRule.Anything.Where(MorphemeRule.O),
                },
            };

            AdTreeBuilder builder = new AdTreeBuilder(patterns);

            Morpheme morpheme = new Morpheme("I") { Attributes = StructuralAttributes.O.Pronoun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("read") { Attributes = StructuralAttributes.I.Verb };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            Assert.AreEqual(1, builder.ActiveAdTrees.Count);
            Assert.IsTrue(builder.ActiveAdTrees[0].Left.Morpheme.Morph == "I" && builder.ActiveAdTrees[0].Left.Morpheme.GrammarCharacter == GrammarCharacter.O);
            Assert.IsTrue(builder.ActiveAdTrees[0].Right.Morpheme.Morph == "read" && builder.ActiveAdTrees[0].Right.Morpheme.GrammarCharacter == GrammarCharacter.I);
            Assert.IsTrue(builder.ActiveAdTrees[0].Morpheme.Morph == "" && builder.ActiveAdTrees[0].Morpheme.GrammarCharacter == GrammarCharacter.Epsilon);
        }

        [Test]
        public void AddMorpheme_the_good_book()
        {
            List<IPattern> patterns = new List<IPattern>()
            {
                new Pattern()
                {
                    MorphemeRule = MorphemeRule.A,
                    AdPositionRule = PatternRule.Anything,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern()
                {
                    MorphemeRule = MorphemeRule.O,
                    AdPositionRule = PatternRule.Anything,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern()
                {
                    MorphemeRule = MorphemeRule.Epsilon,
                    AdPositionRule = PatternRule.Anything,
                    RightRule = PatternRule.Anything.Where(MorphemeRule.O),
                    LeftRule = PatternRule.Anything.Where(MorphemeRule.A),
                },
            };

            AdTreeBuilder builder = new AdTreeBuilder(patterns);

            Morpheme morpheme = new Morpheme("the") { Attributes = StructuralAttributes.A.Determiner.DefiniteArticle };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("good") { Attributes = StructuralAttributes.A.Adjective.Attributive };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("book") { Attributes = StructuralAttributes.O.Noun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            Assert.AreEqual(1, builder.ActiveAdTrees.Count);
            Assert.IsTrue(builder.ActiveAdTrees[0].Left.Morpheme.Morph == "I" && builder.ActiveAdTrees[0].Left.Morpheme.GrammarCharacter == GrammarCharacter.O);
            Assert.IsTrue(builder.ActiveAdTrees[0].Right.Morpheme.Morph == "read" && builder.ActiveAdTrees[0].Right.Morpheme.GrammarCharacter == GrammarCharacter.I);
            Assert.IsTrue(builder.ActiveAdTrees[0].Morpheme.Morph == "" && builder.ActiveAdTrees[0].Morpheme.GrammarCharacter == GrammarCharacter.Epsilon);
        }

        [Test]
        public void AddMorpheme_FindingPatternForAdTreeElement()
        {
            List<IPattern> patterns = new List<IPattern>()
            {
                // Noun
                new Pattern()
                {
                    MorphemeRule = MorphemeRule.O,
                    AdPositionRule = PatternRule.Epsilon,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                // Epsilon
                new Pattern()
                {
                    MorphemeRule = MorphemeRule.Epsilon,
                    AdPositionRule = PatternRule.Anything,
                    RightRule = PatternRule.Anything,
                    LeftRule = PatternRule.Anything,
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
                    MorphemeRule = MorphemeRule.O,
                    AdPositionRule = PatternRule.Anything,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },
                new Pattern()
                {
                    MorphemeRule = MorphemeRule.I,
                    AdPositionRule = PatternRule.Anything,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },
                new Pattern()
                {
                    MorphemeRule = MorphemeRule.A,
                    AdPositionRule = PatternRule.Anything,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },
                new Pattern()
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.First,
                    MorphemeRule = MorphemeRule.Epsilon,
                    AdPositionRule = PatternRule.Anything,
                    RightRule = PatternRule.Anything.Where(MorphemeRule.I),
                    LeftRule = PatternRule.Anything.Where(MorphemeRule.O),
                },
                new Pattern()
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.Second,
                    MorphemeRule = MorphemeRule.Epsilon,
                    AdPositionRule = PatternRule.Anything,
                    RightRule = PatternRule.Anything.Where(MorphemeRule.I),
                    LeftRule = PatternRule.Anything.Where(MorphemeRule.O),
                },
                new Pattern()
                {
                    AdPositionRule = PatternRule.Anything,
                    RightRule = PatternRule.Anything.Where(MorphemeRule.O),
                    LeftRule = PatternRule.Anything.Where(MorphemeRule.A),
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
