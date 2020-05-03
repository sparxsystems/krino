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
        public void I_read()
        {
            List<IPattern> patterns = new List<IPattern>()
            {
                new Pattern()
                {
                    MorphemeRule = MorphemeRule.O,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern()
                {
                    MorphemeRule = MorphemeRule.I,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern()
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.First,
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.I),
                    LeftRule = new PatternRule(MorphemeRule.O)
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
        public void The_good_book()
        {
            List<IPattern> patterns = new List<IPattern>()
            {
                new Pattern()
                {
                    MorphemeRule = MorphemeRule.A,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern()
                {
                    MorphemeRule = MorphemeRule.O,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern()
                {
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.O),
                    LeftRule = new PatternRule(MorphemeRule.A)
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
            Assert.AreEqual("the", builder.ActiveAdTrees[0].Left.Morpheme.Morph);
            Assert.AreEqual("good", builder.ActiveAdTrees[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("book", builder.ActiveAdTrees[0].Right.Right.Morpheme.Morph);
        }

        [Test]
        public void I_read_the_book()
        {
            List<IPattern> patterns = new List<IPattern>()
            {
                new Pattern("O")
                {
                    MorphemeRule = MorphemeRule.O,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },
                new Pattern("I")
                {
                    MorphemeRule = MorphemeRule.I,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },
                new Pattern("A")
                {
                    MorphemeRule = MorphemeRule.A,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },
                new Pattern("1st Valency")
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.First,
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.I),
                    LeftRule = new PatternRule(MorphemeRule.O)
                },
                new Pattern("2nd Valency")
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.Second,
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.I),
                    LeftRule = new PatternRule(MorphemeRule.O)
                },
                new Pattern("A<->O")
                {
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.O),
                    LeftRule = new PatternRule(MorphemeRule.A)
                },
            };

            AdTreeBuilder builder = new AdTreeBuilder(patterns);


            Morpheme morpheme = new Morpheme("I") { Attributes = StructuralAttributes.O.Pronoun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("read") { Attributes = StructuralAttributes.I.Verb.Bivalent };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("the") { Attributes = StructuralAttributes.A.Determiner.DefiniteArticle };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("book") { Attributes = StructuralAttributes.O.Noun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));


            Assert.AreEqual(1, builder.ActiveAdTrees.Count);
            Assert.AreEqual("I", builder.ActiveAdTrees[0].Right.Left.Morpheme.Morph);
            Assert.AreEqual("read", builder.ActiveAdTrees[0].Right.Right.Morpheme.Morph);
            Assert.AreEqual("the", builder.ActiveAdTrees[0].Left.Left.Morpheme.Morph);
            Assert.AreEqual("book", builder.ActiveAdTrees[0].Left.Right.Morpheme.Morph);
        }


        [Test]
        public void I_read_the_book_in_the_room()
        {
            List<IPattern> patterns = new List<IPattern>()
            {
                new Pattern("O")
                {
                    MorphemeRule = MorphemeRule.O,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },
                new Pattern("I")
                {
                    MorphemeRule = MorphemeRule.I,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },
                new Pattern("A")
                {
                    MorphemeRule = MorphemeRule.A,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },
                new Pattern("1st Valency")
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.First,
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.I),
                    LeftRule = new PatternRule(MorphemeRule.O)
                },
                new Pattern("2nd Valency")
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.Second,
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.I),
                    LeftRule = new PatternRule(MorphemeRule.O)
                },
                new Pattern("A<->O")
                {
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.O),
                    LeftRule = new PatternRule(MorphemeRule.A)
                },
                new Pattern("E")
                {
                    MorphemeRule = MorphemeRule.E_Preposition,
                    RightRule = new PatternRule(MorphemeRule.I),
                    LeftRule = new PatternRule(MorphemeRule.O)
                },
            };

            AdTreeBuilder builder = new AdTreeBuilder(patterns);


            Morpheme morpheme = new Morpheme("I") { Attributes = StructuralAttributes.O.Pronoun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("read") { Attributes = StructuralAttributes.I.Verb.Bivalent };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("the") { Attributes = StructuralAttributes.A.Determiner.DefiniteArticle };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("book") { Attributes = StructuralAttributes.O.Noun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("in") { Attributes = StructuralAttributes.E.Preposition };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("the") { Attributes = StructuralAttributes.A.Determiner.DefiniteArticle };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            morpheme = new Morpheme("room") { Attributes = StructuralAttributes.O.Noun };
            Assert.IsTrue(builder.AddMorpheme(morpheme));

            Assert.AreEqual(1, builder.ActiveAdTrees.Count);
            Assert.AreEqual("I", builder.ActiveAdTrees[0].Right.Right.Left.Morpheme.Morph);
            Assert.AreEqual("read", builder.ActiveAdTrees[0].Right.Right.Right.Morpheme.Morph);
            Assert.AreEqual("the", builder.ActiveAdTrees[0].Right.Left.Left.Morpheme.Morph);
            Assert.AreEqual("book", builder.ActiveAdTrees[0].Right.Left.Right.Morpheme.Morph);
        }
    }
}
