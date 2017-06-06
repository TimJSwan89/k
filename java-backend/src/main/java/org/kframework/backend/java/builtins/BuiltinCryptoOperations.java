// Copyright (c) 2016 K Team. All Rights Reserved.
package org.kframework.backend.java.builtins;


import org.apache.commons.codec.DecoderException;
import org.bouncycastle.jcajce.provider.digest.Keccak;
import org.bouncycastle.jcajce.provider.digest.SHA3;
import org.bouncycastle.jcajce.provider.digest.SHA256;
import org.bouncycastle.util.encoders.Hex;
import org.kframework.backend.java.kil.TermContext;
import org.kframework.utils.errorsystem.KEMException;

/**
 * Builtins for Cryptographic Operations
 */
public final class BuiltinCryptoOperations {

    /**
     * Finds the keccak256 digest of the input.
     *
     * @param inputHexString - The String is expected to be formed such that each character in the string
     *                       represents a Hex Value, and can be directly encoded into a byte.
     * @return Output String (256 characters) such that each character represents an encoded Hex Value.
     */
    public static StringToken keccak256(StringToken inputHexString, TermContext context) {
        byte[] bytes = org.apache.commons.codec.binary.StringUtils.getBytesIso8859_1(inputHexString.stringValue());
        Keccak.Digest256 keccakEngine = new Keccak.Digest256();
        byte[] digest = keccakEngine.digest(bytes);
        String digestString = Hex.toHexString(digest);
        return StringToken.of(digestString);
    }

    /**
     * Finds the SHA3 digest of the input.
     *
     * @param inputHexString - The String is expected to be formed such that each character in the string
     *                       represents a Hex Value, and can be directly encoded into a byte.
     * @return Output String (256 characters) such that each character represents an encoded Hex Value.
     */
    public static StringToken sha3256(StringToken inputHexString, TermContext context) {
        byte[] bytes = org.apache.commons.codec.binary.StringUtils.getBytesIso8859_1(inputHexString.stringValue());
        SHA3.Digest256 sha3engine = new SHA3.Digest256();
        byte[] digest = sha3engine.digest(bytes);
        String digestString = Hex.toHexString(digest);
        return StringToken.of(digestString);
    }

    /**
     * Finds the SHA256 digest of the input.
     *
     * @param inputHexString - The String is expected to be formed such that each character in the string
     *                       represents a Hex Value, and can be directly encoded into a byte.
     * @return Output String (256 characters) such that each character represents an encoded Hex Value.
     */
    public static StringToken sha256(StringToken inputHexString, TermContext context) {
        byte[] bytes = org.apache.commons.codec.binary.StringUtils.getBytesIso8859_1(inputHexString.stringValue());
        SHA256.Digest sha2engine = new SHA256.Digest();
        byte[] digest = sha2engine.digest(bytes);
        String digestString = Hex.toHexString(digest);
        return StringToken.of(digestString);
    }

}