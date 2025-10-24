"""
Logging configuration for MCP Flight Search.
"""

import logging
from rich.logging import RichHandler


def setup_logging():
    """Configure and set up logging for the application"""
    logging.basicConfig(
        level=logging.DEBUG,
        format="| %(levelname)-8s | %(name)s | %(message)s",
        datefmt="[%Y-%m-%d %H:%M:%S]",
        handlers=[RichHandler(rich_tracebacks=True)],
        force=True,
    )

    logger = logging.getLogger("flight_search")
    logging.getLogger("uvicorn.access").setLevel(logging.INFO)
    logging.getLogger("uvicorn.error").setLevel(logging.INFO)
    logging.getLogger("httpx").setLevel(logging.INFO)

    return logger


# Create the logger instance for import by other modules
logger = setup_logging()
