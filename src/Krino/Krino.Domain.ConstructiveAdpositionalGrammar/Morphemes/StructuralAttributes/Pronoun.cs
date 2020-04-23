﻿using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributes
{
    /// <summary>
    /// Pronoun (refers to specific people or things e.g. I, me, mine, you, yours ..., they, them) attributes.
    /// </summary>
    public class Pronoun : EnumGroupBase
    {
        public Pronoun(EnumGroupBase parent) : base(parent)
        {
            Subjective = new EnumValue(this);
            Objective = new EnumValue(this);
            Possessive = new EnumValue(this);
            Reflexive = new EnumValue(this);
        }

        /// <summary>
        /// I, you, we, he, she, it and they.
        /// </summary>
        /// <remarks>
        /// They act as a subject of verbs.
        /// </remarks>
        public EnumValue Subjective { get; }

        /// <summary>
        /// me, you, us, him, her, it and them.
        /// </summary>
        /// <remarks>
        /// They act as objects of verbs.
        /// </remarks>
        public EnumValue Objective { get; }

        /// <summary>
        /// mine, yours, hers, his, ours and theirs.
        /// </summary>
        /// <remarks>
        /// They refer to something owned by the speaker or by someone or something previously mentioned.
        /// </remarks>
        public EnumValue Possessive { get; }

        /// <summary>
        /// myself, himself, herslef, itself, ourselves, yourselves and themselves.
        /// </summary>
        /// <remarks>
        /// They refer back to the subject of the phrase in which they are used.
        /// </remarks>
        public EnumValue Reflexive { get; }
    }
}
