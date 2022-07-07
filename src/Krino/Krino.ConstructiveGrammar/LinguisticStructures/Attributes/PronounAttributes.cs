using Krino.ConstructiveGrammar.LinguisticStructures.Attributes.Sememes;
using Krino.Vertical.Utils.Enums;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Attributes
{
    /// <summary>
    /// Pronoun (refers to specific people or things e.g. I, me, mine, you, yours ..., they, them) attributes.
    /// </summary>
    public class PronounAttributes : EnumGroupBase
    {
        public PronounAttributes(EnumGroupBase parent) : base(parent)
        {
            Subjective = new EnumValue(this);
            Objective = new EnumValue(this);
            Possessive = new EnumValue(this);
            Reflexive = new EnumValue(this);
            Interrogative = new EnumValue(this);

            Sememe = new PronounSememes(this);
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

        /// <summary>
        /// Interrogative pronouns are used to ask questions. E.g. what, which.
        /// </summary>
        public EnumValue Interrogative { get; }

        /// <summary>
        /// Semantic attributes.
        /// </summary>
        public PronounSememes Sememe { get; }
    }
}
