"""Generic R code safety helpers shared by review and execution boundaries."""

from __future__ import annotations

import re


DEFAULT_FORBIDDEN_R_CALLS = (
    "system",
    "system2",
    "shell",
    "unlink",
    "file.remove",
    "download.file",
    "install.packages",
)


def find_forbidden_r_calls(
    code: str,
    forbidden_calls: tuple[str, ...] = DEFAULT_FORBIDDEN_R_CALLS,
) -> tuple[str, ...]:
    """Return forbidden R calls found outside comments and string literals."""

    return tuple(call for call, _evidence in find_forbidden_r_call_matches(code, forbidden_calls))


def find_forbidden_r_call_matches(
    code: str,
    forbidden_calls: tuple[str, ...] = DEFAULT_FORBIDDEN_R_CALLS,
    *,
    code_is_stripped: bool = False,
) -> tuple[tuple[str, str], ...]:
    """Return forbidden R calls and matched evidence.

    Set ``code_is_stripped`` when the caller already removed R comments and
    string literals and wants evidence from that prepared text.
    """

    code_for_calls = code if code_is_stripped else r_code_without_comments_and_strings(code)
    matches: list[tuple[str, str]] = []
    for call in forbidden_calls:
        pattern = re.compile(rf"(?<![A-Za-z0-9_.]){re.escape(call)}\s*\(", flags=re.IGNORECASE)
        match = pattern.search(code_for_calls)
        if match:
            matches.append((call, match.group(0)))
    return tuple(matches)


def r_code_without_comments_and_strings(code: str) -> str:
    """Replace R comments and string literals with spaces."""

    result: list[str] = []
    in_string: str | None = None
    escaped = False
    index = 0
    while index < len(code):
        character = code[index]
        if in_string:
            if escaped:
                escaped = False
            elif character == "\\":
                escaped = True
            elif character == in_string:
                in_string = None
            result.append(" ")
            index += 1
            continue
        if character in {"'", '"'}:
            in_string = character
            result.append(" ")
            index += 1
            continue
        if character == "#":
            while index < len(code) and code[index] not in {"\r", "\n"}:
                result.append(" ")
                index += 1
            continue
        result.append(character)
        index += 1
    return "".join(result)
